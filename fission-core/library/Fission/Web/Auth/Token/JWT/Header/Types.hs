-- | JWT Header as specific in RFC 7519

module Fission.Web.Auth.Token.JWT.Header.Types
  ( Header    (..)

  -- * Reexports

  , module Fission.Key.Asymmetric.Algorithm.Types
  ) where

import           Fission.Prelude
import           Fission.SemVer.Types

import           Fission.Key.Asymmetric.Algorithm.Types

import           Fission.Web.Auth.Token.JWT.Header.Typ.Types

data Header = Header
  { typ :: !Typ         -- ^ Standard JWT '"typ"' field
  , alg :: !Algorithm   -- ^ Standard JWT '"alg"' field
  , ucv :: !SemVer      -- ^ UCAN Version, mainly to state assumptions
  } deriving (Show, Eq)

instance Arbitrary Header where
  arbitrary = do
    typ <- arbitrary
    alg <- arbitrary
    ucv <- SemVer 0 <$> ((4 +) <$> arbitrary) <*> arbitrary
    return Header {..}

instance ToJSON Header where
  toJSON Header {..} = object
    [ "typ" .= typ
    , "alg" .= alg
    , "ucv" .= ucv
    ]

instance FromJSON Header where
  parseJSON = withObject "JWT.Header" \obj -> do
    typ <- obj .: "typ"
    alg <- obj .: "alg"
    ucv <- obj .: "ucv"
    return Header {..}
