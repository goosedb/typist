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

instance (Logged a) => IsLabel s (a -> Arg n s) where
  fromLabel = Arg . toLog

data WithTemplate = WithTemplate {rendered :: !Builder.Builder, template :: !Builder.Builder}
  deriving (Show, Eq)

{-# INLINE fmtt #-}
fmtt :: forall str. (Interpolate WithTemplate (Format WithTemplate str), KnownSymbol str) => Format WithTemplate str
fmtt = interpolate (`WithTemplate` fromString t) 0 t mempty
 where
  t = symbolVal @str Proxy

{-# INLINE fmt #-}
fmt :: forall str. (Interpolate Builder.Builder (Format Builder.Builder str), KnownSymbol str) => Format Builder.Builder str
fmt = interpolate id 0 t mempty
 where
  t = symbolVal @str Proxy
