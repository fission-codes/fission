module Fission.Key.Asymmetric.Private.Types (Private (..)) where

import           Fission.Prelude

newtype Private = Private { privateKey :: Text }
  deriving (Eq, Show)
