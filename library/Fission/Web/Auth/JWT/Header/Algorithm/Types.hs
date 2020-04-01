-- | 

module Fission.Web.Auth.JWT.Header.Algorithm.Types (Algorithm (..)) where

import qualified RIO.Text as Text
import           Fission.Prelude


-- FIXME: Replace all call sites with PublicKey.Algorithm!
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
