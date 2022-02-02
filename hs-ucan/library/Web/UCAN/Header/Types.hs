-- | JWT Header as specific in RFC 7519

module Web.UCAN.Header.Types
  ( Header    (..)
  , ucanVersion

  -- * Reexports

  , module Crypto.Key.Asymmetric.Algorithm.Types
  ) where

import           RIO

import           Data.Aeson
import qualified Data.Aeson.Types                      as JSON
import           Test.QuickCheck

import           Crypto.Key.Asymmetric.Algorithm.Types

import           Web.SemVer.Types
import           Web.UCAN.Header.Typ.Types

data Header = Header
  { typ :: Typ       -- ^ Standard JWT '"typ"' field
  , alg :: Algorithm -- ^ Standard JWT '"alg"' field
  , ucv :: SemVer    -- ^ UCAN Version, mainly to state assumptions
  } deriving (Show, Eq)

instance Arbitrary Header where
  arbitrary = do
    typ <- arbitrary
    alg <- arbitrary
    let ucv = ucanVersion
    return Header {..}

instance ToJSON Header where
  toJSON Header {..} = object
    [ "typ" .= typ
    , "alg" .= alg
    , "ucv" .= ucv
    ]

instance FromJSON Header where
  parseJSON val =
        parseWithUcv val
    <|> parseWithUav val


parseWithUcv :: Value -> JSON.Parser Header
parseWithUcv = withObject "JWT.Header" \obj -> do
  typ <- obj .: "typ"
  alg <- obj .: "alg"
  ucv <- obj .: "ucv"
  return Header {..}


parseWithUav :: Value -> JSON.Parser Header
parseWithUav = withObject "JWT.Header" \obj -> do
  typ           <- obj .: "typ"
  alg           <- obj .: "alg"
  (_ :: SemVer) <- obj .: "uav"
  let ucv = SemVer 0 3 1 -- we assume uav ucans are ucv 0.3.1
  return Header {..}


ucanVersion :: SemVer
ucanVersion = SemVer 0 7 0
