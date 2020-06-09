module Fission.CLI.Config.Connected.Error.Types (Error (..)) where

import           Fission.Prelude

data Error
  = NoKeyFile
  | NotRegistered
  | CannotConnect
  | PeersNotFound
  | NoApp
  deriving (Eq, Show, Exception)
