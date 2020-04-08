module Fission.Web.Auth.JWT.Claims.Types
  ( Claims (..)
  , Attenuation (..)
  ) where

import           Data.Time.Clock.POSIX

import           Fission.Prelude
import           Fission.User.DID.Types

data Claims = Claims
  { iss        :: !DID
  , exp        :: !UTCTime
  , nbf        :: !(Maybe UTCTime)
  } deriving Show

instance Eq Claims where
  jwtA == jwtB =
                       (iss jwtA == iss jwtB)
    &&      (roundUTC (exp jwtA) ==      roundUTC (exp jwtB))
    && (fmap roundUTC (nbf jwtA) == fmap roundUTC (nbf jwtB))

instance Arbitrary Claims where
  arbitrary = do
    iss <- arbitrary
    exp <- fromSeconds . toSeconds <$> arbitrary
    nbf <- arbitrary
    return Claims {..}

instance ToJSON Claims where
  toJSON Claims {..} = object
    [ "iss" .= iss
    , "nbf" .= fmap toSeconds nbf
    , "exp" .= toSeconds exp
    ]

-- FIXME move to a time module
roundUTC :: UTCTime -> UTCTime
roundUTC = fromSeconds . toSeconds

toSeconds :: UTCTime -> Int
toSeconds = round . utcTimeToPOSIXSeconds

instance FromJSON Claims where
  parseJSON = withObject "JWT.Payload" \obj -> do
    iss <- obj .: "iss"
    nbf <- fmap fromSeconds <$> obj .: "nbf"
    exp <-      fromSeconds <$> obj .: "exp"

    return Claims {..}
 
fromSeconds :: Int -> UTCTime
fromSeconds n = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromIntegral n

newtype Attenuation = Attenuation (Map FFSPath Right)

data Right = Right Permission FFSPath
  deriving (Eq, Show)

data Permission
  = ReadOnly
  | AppendOnly
  | SuperUser
  deriving (Eq, Ord, Show)

data FFSPath -- TODO move to FFS description
  = Root
  | Private   FFSPath
  | Public    FFSPath
  | NamedNode Text
  deriving (Eq, Show)
