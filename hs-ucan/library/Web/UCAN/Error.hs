module Web.UCAN.Error (Error (..)) where

import           Data.Aeson
import           RIO

import qualified Web.UCAN.Claims.Error    as Claims
import qualified Web.UCAN.Header.Error    as Header
import qualified Web.UCAN.Resolver        as Resolver
import qualified Web.UCAN.Signature.Error as Signature
import qualified Web.UCAN.Witness.Error   as Witness

data Error
  = ParseError        String
  | HeaderError       Header.Error
  | ClaimsError       Claims.Error
  | SignatureError    Signature.Error
  | WitnessError      Witness.Error
  | ResolverError     Resolver.Error
  deriving (Exception, Eq, Show)

instance ToJSON Error where
  toJSON = String . textDisplay

instance Display Error where
  display = \case
    ParseError        err -> "Could not parse JWT: "         <> fromString err
    HeaderError       err -> "JWT header error: "            <> display err
    SignatureError    err -> "JWT signature error: "         <> display err
    ClaimsError       err -> "JWT claims error: "            <> display err
    WitnessError      err -> "JWT witness error: "           <> display err
    ResolverError     err -> "Unable to resolve CID proof: " <> display err
