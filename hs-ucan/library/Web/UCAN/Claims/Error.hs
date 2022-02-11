module Web.UCAN.Claims.Error (Error (..)) where

import           RIO

import qualified Web.UCAN.Witness.Error as Witness

data Error
  = Expired
  | TooEarly
  | IncorrectSender
  | IncorrectReceiver
  | WitnessError Witness.Error
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    Expired             -> "Expired"
    TooEarly            -> "Use too early"
    IncorrectSender     -> "Incorrect sender"
    IncorrectReceiver   -> "Incorrect receiver"
    WitnessError resErr -> "Witness error: " <> display resErr
