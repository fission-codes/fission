module Fission.Web.Auth.Token.JWT.Claims.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

import qualified Fission.Web.Auth.Token.JWT.Proof.Error as Proof

data Error
  = Expired
  | TooEarly
  | IncorrectReceiver
  | ProofError Proof.Error
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    Expired               -> "Expired"
    TooEarly              -> "Use too early"
    IncorrectReceiver     -> "Incorrect receiver"
    ProofError resErr     -> "Proof error: " <> display resErr

instance ToServerError Error where
  toServerError = \case
    ProofError    err -> toServerError err
    IncorrectReceiver -> err401 { errBody = displayLazyBS IncorrectReceiver }
    Expired           -> err401 { errBody = displayLazyBS Expired }
    TooEarly          -> err401 { errBody = displayLazyBS TooEarly }
