-- | JWT Header as specific in RFC 7519

module Web.JWT.Header.Types
  ( Header    (..)

  -- * Reexports

  , module Fission.Key.Asymmetric.Algorithm.Types
  ) where

import           Fission.Prelude

import           Crypto.Key.Asymmetric.Algorithm.Types

import           Web.JWT.Header.Cty.Types
import           Web.JWT.Header.Typ.Types
import           Web.SemVer.Types

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
