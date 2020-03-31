-- | 

module Fission.Web.Auth.JWT.Payload.Types (Payload (..)) where

import           Fission.Prelude
import           Fission.PublicKey.Types

data Payload = Payload
  { iss        :: !PublicKey
  , exp        :: !UTCTime
  , nbf        :: !(Maybe UTCTime)
  } deriving (Eq, Show)

instance ToJSON Payload where
  toJSON Payload {..} = object
    [ "iss" .= toJSON iss
    , "nbf" .= toJSON nbf
    , "exp" .= toJSON exp
    ]

instance FromJSON Payload where
  parseJSON = withObject "JWT.Payload" \obj -> do
    iss <- obj .: "iss"
    nbf <- obj .: "nbf"
    exp <- obj .: "exp"

    return Payload {..}
