module Fission.CLI.Linking.PIN.Types (PIN (..)) where

import           Fission.Prelude

newtype PIN = PIN Text
  deriving newtype (Show, Display, Eq, ToJSON, FromJSON)
