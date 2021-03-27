module Fission.CLI.PIN
  ( create
  , module Fission.CLI.PIN.Types
  , module Fission.CLI.PIN.Payload.Types
  ) where

import           Fission.Prelude

import           Fission.CLI.PIN.Payload.Types
import           Fission.CLI.PIN.Types

create :: MonadIO m => m PIN
create = liftIO $ generate arbitrary
