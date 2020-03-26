module Fission.CLI.Config.Base
  ( runBase
  , module Fission.CLI.Config.Base.Types
  ) where

import Fission.Prelude
import Fission.CLI.Config.Base.Types

runBase :: MonadIO m => BaseConfig -> FissionBase a -> m a
runBase cfg actions =
  actions
    |> unwrapFissionBase
    |> runRIO cfg
