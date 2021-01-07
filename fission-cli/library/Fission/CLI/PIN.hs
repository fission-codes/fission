module Fission.CLI.PIN
  ( create
  , module Fission.CLI.PIN.Types
  ) where

import           Fission.Prelude

import           Fission.CLI.PIN.Types

create :: MonadIO m => m PIN
create = liftIO $ generate arbitrary
