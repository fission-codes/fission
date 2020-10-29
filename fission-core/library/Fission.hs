module Fission
  ( runFission
  , module Fission.Types
  , module Fission.Config.Types
  ) where

import           Fission.Config.Types
import           Fission.Prelude
import           Fission.Types

-- | Run actions described by a @Fission@ type
runFission :: MonadIO m => Config -> Fission a -> m a
runFission cfg actions = runRIO cfg $ unFission actions
