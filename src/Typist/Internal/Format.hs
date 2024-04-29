{-# LANGUAGE PolyKinds  #-}
module Typist.Internal.Format (module Rec, module Typist.Internal.Format) where

import qualified Data.Text.Lazy.Builder as Builder
import GHC.TypeLits (
  ConsSymbol,
  Nat,
  Symbol,
  TypeError,
  UnconsSymbol,
  type (+), ErrorMessage (..), symbolVal,
 )
import Data.Record.Anon as Rec
import Data.Record.Anon.Simple (Record, get, empty)
import Data.String (IsString(..))
import Data.Record.Anon.Overloading (fromLabel)
import GHC.TypeNats ( natVal, KnownNat )

type family Format (str :: Symbol) where
  Format str = ContFormat 0 (UnconsSymbol str)

newtype Arg (n :: Nat) = Arg Builder.Builder

type family ContFormat (n :: Nat) (a :: Maybe (Char, Symbol)) where
  ContFormat n ('Just '( '\\', rest)) = SkipOne (n + 1) (UnconsSymbol rest)
  ContFormat n ('Just '( '#', rest)) = TryGetArg n (UnconsSymbol rest)
  ContFormat n ('Just '(a, rest)) = ContFormat (n + 1) (UnconsSymbol rest)
  ContFormat n 'Nothing = '[]

type family TryGetArg n rest where
  TryGetArg n ('Just '( '{', rest)) =
    TakeName (UnconsSymbol rest) ':= Arg n ':
    ContFormat (n + 2) (UnconsSymbol (SkipName (UnconsSymbol rest)))
  TryGetArg n 'Nothing = ContFormat n 'Nothing
  TryGetArg n ('Just '(a, rest)) = ContFormat (n + 2) (UnconsSymbol rest)

type family SkipOne (n :: Nat) (s :: Maybe (Char, Symbol)) where
  SkipOne n 'Nothing = ContFormat n 'Nothing
  SkipOne n ('Just '(a, rest)) = ContFormat (n + 1) (UnconsSymbol rest)

type family TakeName (a :: Maybe (Char, Symbol)) :: Symbol where
  TakeName ('Just '( '}', rest)) = ""
  TakeName ('Just '(a, rest)) = ConsSymbol a (TakeName (UnconsSymbol rest))
  TakeName 'Nothing = TypeError ('Text "Expected '}' but EOF found. Close placeholder with '}'. Example: #{name}")

type family SkipName (a :: Maybe (Char, Symbol)) :: Symbol where
  SkipName ('Just '( '}', rest)) = rest
  SkipName ('Just '(a, rest)) = SkipName (UnconsSymbol rest)
  SkipName 'Nothing = ""

fmt :: forall str. (KnownSymbol str, Interpolate (Format str) (Format str)) => (Record '[] -> Record (Format str)) -> Builder.Builder
fmt record = interpolate @(Format str) @(Format str) (record empty) 0 (symbolVal (Proxy @str)) mempty

class Interpolate args rest where
  interpolate :: Record args -> Int -> String -> Builder.Builder -> Builder.Builder

instance Interpolate args '[] where
  {-# INLINE interpolate #-}
  interpolate _ _ string acc = acc <> fromString string

instance (Interpolate args as, RowHasField n args (Arg i), KnownSymbol n, KnownHash n, KnownNat i) => Interpolate args ((n :: Symbol) ':= Arg i ': as) where
  {-# INLINE interpolate #-}
  interpolate record start string acc =
    interpolate @args @as
      record
      (nVal + 2)
      (drop 1 $ dropWhile (/= '}') $ drop (diff + 3) string)
      (acc <> fromString (take diff string) <> s)
     where
    Arg s = get (fromLabel @n) record
    nVal = fromIntegral $ natVal (Proxy @i)
    diff = nVal - start
