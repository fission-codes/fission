module Fission.Web.Auth.Token.JWT.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

import qualified Fission.Web.Auth.Token.JWT.Header.Error    as Header
import qualified Fission.Web.Auth.Token.JWT.Claims.Error    as Claims
import qualified Fission.Web.Auth.Token.JWT.Signature.Error as Signature

data Error
  = ParseError
  | HeaderError    Header.Error
  | ClaimsError    Claims.Error
  | SignatureError Signature.Error
  deriving ( Exception
           , Eq
           , Show
           )

instance ToJSON Error where
  toJSON = String . textDisplay

instance Display Error where
  display = \case
    ParseError         -> "Could not parse JWT"
    HeaderError    err -> "JWT header error: "    <> display err
    SignatureError err -> "JWT signature error: " <> display err
    ClaimsError    err -> "JWT claims error: "    <> display err

instance ToServerError Error where
  toServerError = \case
    ParseError         -> err400 { errBody = displayLazyBS ParseError }
    HeaderError    err -> toServerError err
    ClaimsError    err -> toServerError err
    SignatureError err -> toServerError err
