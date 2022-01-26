-- | App configuration for ID and auth

module Fission.Web.Server.Environment.Auth.Types (Environment (..)) where

import           Fission.Prelude

import           Web.DID.Types

newtype Environment = Environment { fissionDID :: DID }
  deriving (Show, Eq)

instance FromJSON Environment where
  parseJSON = withObject "Auth.Environment" \obj -> do
    fissionDID <- obj .: "fission_did"
    return Environment {..}
