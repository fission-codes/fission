module Fission.Time.Seconds.Types (Seconds (..)) where

import           Fission.Prelude

newtype Seconds m n = Seconds { getSeconds :: m n }
  deriving (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

instance Display (m a) => Display (Seconds m a) where
  display (Seconds s) = display s <> "s"

instance ToPrefixed prefix n => ToPrefixed (Seconds prefix) n where
  toPrefixed a = Seconds $ toPrefixed a

instance FromPrefixed prefix n => FromPrefixed (Seconds prefix) n where
  fromPrefixed (Seconds prefixed) = fromPrefixed prefixed
