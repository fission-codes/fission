-- |

module Fission.Web.Auth.JWT.Header.Types
  ( Header    (..)
  , Typ       (..)
  , Algorithm (..)
  ) where

import qualified RIO.Text as Text
import           Fission.Prelude

data Header = Header
  { typ :: !Typ
  , alg :: !Algorithm
  } deriving (Show, Eq)

instance ToJSON Header where
  toJSON Header {..} = object
    [ "typ" .= typ
    , "alg" .= alg
    ]

instance FromJSON Header where
  parseJSON = withObject "JWT.Header" \obj -> do
    typ <- obj .: "typ"
    alg <- obj .: "alg"
    return Header {..}

data Typ
  = JWT
  deriving ( Eq
           , Read
           , Show
           )

instance FromJSON Typ where
  parseJSON = withText "JWT.Typ" \case
    "JWT" -> return JWT
    other -> fail (Text.unpack other <> " is not an acceptable JWT typ")

instance ToJSON Typ where
  toJSON JWT = String "JWT"

data Algorithm
  = RS256
  | Ed25519
  deriving ( Eq
           , Read
           , Show
           )

instance Display Algorithm where
  display RS256   = "RS256"
  display Ed25519 = "Ed25519"

instance ToJSON Algorithm where
  toJSON = String . textDisplay

instance FromJSON Algorithm where
  parseJSON = withText "JWT.Algorithm" \case
    "RS256"   -> return RS256
    "Ed25519" -> return Ed25519
    other     -> fail (Text.unpack other <> " is not a valid JWT algorithm")
