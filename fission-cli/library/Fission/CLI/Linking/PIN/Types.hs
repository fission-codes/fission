-- |

module Fission.CLI.Linking.PIN.Types where

import           Fission.Prelude

newtype PIN = PIN Text
  deriving newtype (Show, Display, Eq, ToJSON, FromJSON)
