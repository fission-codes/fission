module Fission.Key.IV.Error (GenError (..)) where

import           Fission.Prelude

data GenError = GenError
  deriving (Show, Eq, Exception)

instance Display GenError where
  display _ = "Unable to generate valid IV"
