{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Typist.Logged where

import Control.Applicative (ZipList)
import Control.Exception (Exception (..), SomeException (..))
import Data.Complex (Complex)
import Data.Data (Proxy (..))
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Const
import Data.Functor.Identity (Identity)
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Product)
import Data.Ord (Down)
import Data.Ratio
import Data.Semigroup (First, Last, Max, Min, Sum)
import qualified Data.Text as Strict
import Data.Text.Internal.Builder (fromString)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Tuple (Solo)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Typist.Internal.Format (Arg (..), Format, Interpolate (..))
import Data.Record.Anon.Simple ( empty, Record, insert )
import Data.Record.Anon hiding (Product)

class Logged a where
  default toLog :: (Show a) => a -> Builder.Builder
  toLog :: a -> Builder.Builder
  toLog = fromString . show

type family Constraints list :: Constraint where
  Constraints '[] = ()
  Constraints (a ': as) = (Show a, Logged a, Constraints as)

newtype Unquoted s = Unquoted {getUnquoted :: s}

instance Logged Int
instance Logged Integer
instance Logged Float
instance Logged Double
instance Logged Bool
instance Logged Char
instance {-# OVERLAPPING #-} Logged String
instance (Constraints '[a]) => Logged [a]
instance (Constraints '[a, b]) => Logged (a, b)
instance (Constraints '[a, b, c]) => Logged (a, b, c)
instance (Constraints '[a, b, c, d]) => Logged (a, b, c, d)
instance (Constraints '[a, b, c, d, e]) => Logged (a, b, c, d, e)
instance (Constraints '[a, b, c, d, e, f]) => Logged (a, b, c, d, e, f)
instance (Constraints '[a, b, c, d, e, f, g]) => Logged (a, b, c, d, e, f, g)
instance (Logged a, Show a) => Logged (Const a b)
instance (Logged a, Show a) => Logged (Maybe a)
instance (Constraints '[a, b]) => Logged (Either a b)
instance (Constraints '[a]) => Logged (Ratio a)
instance (Constraints '[a]) => Logged (NonEmpty a)
instance (Constraints '[a]) => Logged (Sum a)
instance (Constraints '[a]) => Logged (Product a)
instance (Constraints '[a]) => Logged (Min a)
instance (Constraints '[a]) => Logged (Max a)
instance (Constraints '[a]) => Logged (First a)
instance (Constraints '[a]) => Logged (Last a)
instance (Constraints '[a]) => Logged (Down a)
instance (Constraints '[a]) => Logged (Identity a)
instance (Constraints '[a]) => Logged (Complex a)
instance Logged ()
instance (Constraints '[a]) => Logged (ZipList a)
instance (HasResolution a) => Logged (Fixed a)
instance (Constraints '[a]) => Logged (Solo a)
instance Logged (Unquoted String) where
  toLog = fromString . getUnquoted

instance Logged (Unquoted Strict.Text) where
  toLog = Builder.fromText . getUnquoted

instance Logged (Unquoted Lazy.Text) where
  toLog = Builder.fromLazyText . getUnquoted

instance Logged (Unquoted Builder.Builder) where
  toLog = getUnquoted

instance Logged SomeException where
  toLog (SomeException e) = fromString $ displayException e


{-# INLINE fmt #-}
fmt :: forall str. (KnownSymbol str, Interpolate (Format str) (Format str)) => (Record '[] -> Record (Format str)) -> Builder.Builder
fmt record = interpolate @(Format str) @(Format str) (record empty) 0 (symbolVal (Proxy @str)) mempty

{-# INLINE (@=) #-}
(@=) :: forall a n r i. (KnownSymbol n, KnownHash n, Logged a) => Field n -> a -> (Record r -> Record (n ':= Arg i : r))
_ @= b = insert (fromLabel @n) (Arg $ toLog b)