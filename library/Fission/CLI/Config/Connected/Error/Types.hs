module Fission.CLI.Config.Connected.Error.Types (Error (..)) where

import           Fission.Prelude

data Error
  = NoKeyFile
  | NotRegistered
  | CannotConnect
  | PeersNotFound
  deriving (Eq, Show, Exception)
