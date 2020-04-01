-- | 

module Fission.Web.Auth.JWT.Claims.Types (Claims (..)) where

import           Data.Time.Clock.POSIX

import           Fission.Prelude
import           Fission.User.DID.Types

data Claims = Claims
  { iss        :: !DID
  , exp        :: !UTCTime
  , nbf        :: !(Maybe UTCTime)
  } deriving (Eq, Show)

instance ToJSON Claims where
  toJSON Claims {..} = object
    [ "iss" .= iss
    , "nbf" .= fmap toSeconds nbf
    , "exp" .= toSeconds exp
    ]
    where
      toSeconds :: UTCTime -> Int
      toSeconds = round . utcTimeToPOSIXSeconds

instance FromJSON Claims where
  parseJSON = withObject "JWT.Payload" \obj -> do
    iss <- obj .: "iss"
    nbf <- obj .: "nbf"
    exp <- obj .: "exp"

    return Claims {..}
