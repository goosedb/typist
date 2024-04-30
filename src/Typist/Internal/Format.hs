{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Typist.Internal.Format (module Typist.Internal.Format) where

import Data.Data (Proxy (..))
import Data.String (IsString (..))
import qualified Data.Text.Lazy.Builder as Builder
import GHC.TypeLits (
  ConsSymbol,
  ErrorMessage (..),
  KnownSymbol,
  Nat,
  Symbol,
  TypeError,
  UnconsSymbol,
  symbolVal,
  type (+),
 )
import GHC.TypeNats (KnownNat, natVal)

type family Format (str :: Symbol) where
  Format str = ContFormat 0 (UnconsSymbol str)

newtype Arg (n :: Nat) (s :: Symbol) = Arg Builder.Builder

type family ContFormat (n :: Nat) (a :: Maybe (Char, Symbol)) where
  ContFormat n ('Just '( '\\', rest)) = SkipOne (n + 1) (UnconsSymbol rest)
  ContFormat n ('Just '( '#', rest)) = TryGetArg n (UnconsSymbol rest)
  ContFormat n ('Just '(a, rest)) = ContFormat (n + 1) (UnconsSymbol rest)
  ContFormat n 'Nothing = '[]

type family TryGetArg n rest where
  TryGetArg n ('Just '( '{', rest)) =
    Arg n (TakeName (UnconsSymbol rest))
      ': ContFormat (n + 2) (UnconsSymbol (SkipName (UnconsSymbol rest)))
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

class Interpolate args where
  interpolate :: Rec args -> Int -> String -> Builder.Builder -> Builder.Builder

instance Interpolate '[] where
  {-# INLINE interpolate #-}
  interpolate _ _ string acc = acc <> fromString string

instance (Interpolate args, KnownNat i) => Interpolate (Arg i n ': args) where
  {-# INLINE interpolate #-}
  interpolate (Arg s :& record) start string acc =
    interpolate @args
      record
      (nVal + 2)
      (drop 1 $ dropWhile (/= '}') $ drop (diff + 3) string)
      (acc <> fromString (take diff string) <> s)
   where
    nVal = fromIntegral $ natVal (Proxy @i)
    diff = nVal - start

data Rec as where
  RNil :: Rec '[]
  (:&) :: Arg n s -> Rec ns -> Rec (Arg n s ': ns)


-- | See usage example next to @'Typist.TextShow.#='@ at "Typist.TextShow"
{-# INLINE fmt #-}
fmt :: forall str. (KnownSymbol str, Interpolate (Format str)) => (Rec '[] -> Rec (Format str)) -> Builder.Builder
fmt record_ = interpolate @(Format str) (record_ RNil) 0 (symbolVal (Proxy @str)) mempty
