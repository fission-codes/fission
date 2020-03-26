module Fission.CLI.Prompt.Error.Types (Err (..)) where

import           Fission.Prelude

data Err = RequiredField
  deriving (Show, Exception)
