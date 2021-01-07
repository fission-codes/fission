module Fission.CLI.PubSub.Secure.Connection.Types (Connection (..)) where

import qualified Fission.CLI.PubSub.Class as Insecure

data Connection m cipher = Connection
  { conn :: Insecure.Connection m
  , key  :: cipher
  }
