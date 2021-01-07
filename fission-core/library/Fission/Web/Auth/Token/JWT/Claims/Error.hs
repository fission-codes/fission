module Fission.Web.Auth.Token.JWT.Claims.Error (Error (..)) where

import           Fission.Prelude

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
