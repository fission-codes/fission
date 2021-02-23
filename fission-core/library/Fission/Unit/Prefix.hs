module Fission.Unit.Prefix
  ( convert
  , module Fission.Unit.Prefix.Class
  , module Fission.Unit.Prefix.Types
  ) where

import           RIO

import           Fission.Unit.Prefix.Class
import           Fission.Unit.Prefix.Types

convert :: (FromPrefixed n a, ToPrefixed m a) => n a -> m a
convert = toPrefixed . fromPrefixed
