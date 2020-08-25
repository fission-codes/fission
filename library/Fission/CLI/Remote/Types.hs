module Fission.CLI.Remote.Types (Remote (..)) where

import           Servant.Client

import           Fission.Prelude

-- | Remote environment
data Remote
  = Production
  | Staging
  | Development
  | FullMock
  | Custom !BaseUrl
  deriving (Show, Eq)
