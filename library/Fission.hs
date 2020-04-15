module Fission
  ( runFission
  , module Fission.Types
  , module Fission.Config.Types
  ) where

import Fission.Prelude
import Fission.Types
import Fission.Config.Types

-- | Run actions described by a @Fission@ type
runFission :: MonadIO m => Config -> Fission a -> m a
runFission cfg actions = runRIO cfg $ unFission actions
