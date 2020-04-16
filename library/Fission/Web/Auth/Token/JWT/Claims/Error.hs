module Fission.Web.Auth.Token.JWT.Claims.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

import qualified Fission.Web.Auth.Token.JWT.Proof.Error as Proof

data Error
  = Expired
  | TooEarly
  | ProofError Proof.Error
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    Expired               -> "Expired"
    TooEarly              -> "Use too early"
    ProofError resErr     -> "Proof error: " <> display resErr

instance ToServerError Error where
  toServerError = \case
    ProofError err -> toServerError err
    Expired        -> err410 { errBody = displayLazyBS Expired }
    TooEarly       -> ServerError { errHTTPCode     = 425
                                  , errReasonPhrase = show TooEarly
                                  , errBody         = ""
                                  , errHeaders      = []
                                  }
