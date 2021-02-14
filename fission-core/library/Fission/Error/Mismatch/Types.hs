module Fission.Error.Mismatch.Types (Mismatch (..)) where

import           Fission.Prelude

data Mismatch a = Mismatch
  deriving (Eq, Show, Exception)
