module Web.UCAN.Error (Error (..)) where

import           Data.Aeson
import           RIO

import qualified Web.UCAN.Claims.Error    as Claims
import qualified Web.UCAN.Header.Error    as Header
import qualified Web.UCAN.Proof.Error     as Proof
import qualified Web.UCAN.Resolver        as Resolver
import qualified Web.UCAN.Signature.Error as Signature


data Error
  = ParseError      String
  | HeaderError     Header.Error
  | ClaimsError     Claims.Error
  | SignatureError  Signature.Error
  | ProofError      Proof.Error
  | ResolverError   Resolver.Error
  deriving (Exception, Eq, Show)

instance ToJSON Error where
  toJSON = String . textDisplay

instance Display Error where
  display = \case
    ParseError        err -> "Could not parse JWT: "         <> fromString err
    HeaderError       err -> "JWT header error: "            <> display err
    SignatureError    err -> "JWT signature error: "         <> display err
    ClaimsError       err -> "JWT claims error: "            <> display err
    ProofError        err -> "JWT proof error: "             <> display err
    ResolverError     err -> "Unable to resolve CID proof: " <> display err
