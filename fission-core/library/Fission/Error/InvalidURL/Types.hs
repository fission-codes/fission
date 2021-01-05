module Fission.Error.InvalidURL.Types (InvalidURL (..)) where

import           Fission.Prelude

data InvalidURL = InvalidURL
  deriving (Eq, Show)

instance Display InvalidURL where
  textDisplay _ = "Invalid URL"

