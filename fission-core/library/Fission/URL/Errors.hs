-- FIXME rename tyo InvalidURL
module Fission.URL.Errors (InvalidURL (..)) where

import           Fission.Prelude

data InvalidURL = InvalidURL
  deriving (Eq, Show)

instance Display InvalidURL where
  textDisplay _ = "Invalid URL"

