-- | JWT Header as specific in RFC 7519

module Web.UCAN.Header.Types
  ( Header    (..)

  -- * Reexports

  , module Crypto.Key.Asymmetric.Algorithm.Types
  ) where

import           RIO

import           Data.Aeson
import           Test.QuickCheck

import           Crypto.Key.Asymmetric.Algorithm.Types

import           Web.SemVer.Types
import           Web.UCAN.Header.Cty.Types
import           Web.UCAN.Header.Typ.Types

data Header = Header
  { typ :: Typ       -- ^ Standard JWT '"typ"' field
  , alg :: Algorithm -- ^ Standard JWT '"alg"' field
  , cty :: Maybe Cty -- ^ Standard JWT '"cty"' field. Set to '"JWT"' if there's a recursive JWT in the claims
  , uav :: SemVer    -- ^ UCAN Version, mainly to state assumptions
  } deriving (Show, Eq)

instance Arbitrary Header where
  arbitrary = do
    typ <- arbitrary
    alg <- arbitrary
    cty <- arbitrary
    uav <- SemVer 0 <$> ((1 +) <$> arbitrary) <*> arbitrary
    return Header {..}

instance ToJSON Header where
  toJSON Header {..} = object
    [ "typ" .= typ
    , "alg" .= alg
    , "cty" .= cty
    , "uav" .= uav
    ]

instance FromJSON Header where
  parseJSON = withObject "JWT.Header" \obj -> do
    typ <- obj .:  "typ"
    alg <- obj .:  "alg"
    cty <- obj .:? "cty"
    uav <- obj .:  "uav"
    return Header {..}
