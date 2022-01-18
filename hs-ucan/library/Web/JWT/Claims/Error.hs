module Web.JWT.Claims.Error (Error (..)) where

import           RIO

import qualified Web.JWT.Proof.Error as Proof

data Error
  = Expired
  | TooEarly
  | IncorrectSender
  | IncorrectReceiver
  | ProofError Proof.Error
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    Expired           -> "Expired"
    TooEarly          -> "Use too early"
    IncorrectSender   -> "Incorrect sender"
    IncorrectReceiver -> "Incorrect receiver"
    ProofError resErr -> "Proof error: " <> display resErr
