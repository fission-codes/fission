module Fission
  ( runFission
  , module Fission.Types
  , module Fission.Config.Types
  ) where

import Fission.Prelude
import Fission.Types
import Fission.Config.Types

runFission :: MonadIO m => Config -> Fission a -> m a
runFission cfg actions =
  actions
    |> unwrapFission
    |> runRIO cfg
