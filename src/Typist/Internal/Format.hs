module Typist.Internal.Format where

import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import qualified Data.Text.Lazy.Builder as Builder
import GHC.TypeError (ErrorMessage (..))
import GHC.TypeLits (
  ConsSymbol,
  KnownSymbol,
  Nat,
  Symbol,
  TypeError,
  UnconsSymbol,
  type (+),
 )
import GHC.TypeNats (KnownNat, natVal)

type family Format r (str :: Symbol) :: Type where
  Format r str = ContFormat r 0 (UnconsSymbol str)

newtype Arg (n :: Nat) (s :: Symbol) = Arg Builder.Builder

type family ContFormat r (n :: Nat) (a :: Maybe (Char, Symbol)) where
  ContFormat r n (Just '( '\\', rest)) = SkipOne r (n + 1) (UnconsSymbol rest)
  ContFormat r n (Just '( '#', rest)) = TryGetArg r n (UnconsSymbol rest)
  ContFormat r n (Just '(a, rest)) = ContFormat r (n + 1) (UnconsSymbol rest)
  ContFormat r n Nothing = r

type family TryGetArg r n rest where
  TryGetArg r n (Just '( '{', rest)) =
    Arg n (TakeName (UnconsSymbol rest)) ->
    ContFormat r (n + 2) (UnconsSymbol (SkipName (UnconsSymbol rest)))
  TryGetArg r n Nothing = ContFormat r n Nothing
  TryGetArg r n (Just '(a, rest)) = ContFormat r (n + 2) (UnconsSymbol rest)

type family SkipOne r (n :: Nat) (s :: Maybe (Char, Symbol)) where
  SkipOne r n Nothing = ContFormat r n Nothing
  SkipOne r n (Just '(a, rest)) = ContFormat r (n + 1) (UnconsSymbol rest)

type family TakeName (a :: Maybe (Char, Symbol)) :: Symbol where
  TakeName (Just '( '}', rest)) = ""
  TakeName (Just '(a, rest)) = ConsSymbol a (TakeName (UnconsSymbol rest))
  TakeName 'Nothing = TypeError ('Text "Expected '}' but EOF found. Close placeholer with '}'. Example: #{name}")

type family SkipName (a :: Maybe (Char, Symbol)) :: Symbol where
  SkipName (Just '( '}', rest)) = rest
  SkipName (Just '(a, rest)) = SkipName (UnconsSymbol rest)
  SkipName Nothing = ""

class Interpolate cont f where
  interpolate :: (Builder.Builder -> cont) -> Int -> String -> Builder.Builder -> f

instance Interpolate cont cont where
  {-# INLINE interpolate #-}
  interpolate cont _ string acc = cont (acc <> fromString string)

instance (KnownNat n, KnownSymbol s, Interpolate cont f) => Interpolate cont (Arg n s -> f) where
  {-# INLINE interpolate #-}
  interpolate cont start string acc (Arg s) =
    interpolate
      cont
      (nVal + 2)
      (drop 1 $ dropWhile (/= '}') $ drop (diff + 3) string)
      (acc <> fromString (take diff string) <> s)
   where
    nVal = fromIntegral $ natVal (Proxy @n)
    diff = nVal - start
