module Fission.Web.Auth.Token.JWT.Error (Error (..)) where

import qualified RIO.Text                                   as Text

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

import qualified Fission.Web.Auth.Token.JWT.Claims.Error    as Claims
import qualified Fission.Web.Auth.Token.JWT.Header.Error    as Header
import qualified Fission.Web.Auth.Token.JWT.Signature.Error as Signature

data Error
  = ParseError     String
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
  textDisplay = \case
    ParseError     str -> "Could not parse JWT: " <> Text.pack str
    HeaderError    err -> "JWT header error: "    <> textDisplay err
    SignatureError err -> "JWT signature error: " <> textDisplay err
    ClaimsError    err -> "JWT claims error: "    <> textDisplay err

instance ToServerError Error where
  toServerError = \case
    ParseError     str -> err400 { errBody = displayLazyBS $ ParseError str }
    HeaderError    err -> toServerError err
    ClaimsError    err -> toServerError err
    SignatureError err -> toServerError err
