module Web.UCAN.Claims.Error (Error (..)) where

import           RIO


data Error
  = Expired
  | TooEarly
  | IncorrectReceiver
  | MissingExpectedFact
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    Expired             -> "Expired"
    TooEarly            -> "Use too early"
    IncorrectReceiver   -> "Incorrect receiver"
    MissingExpectedFact -> "Expected fact not present"
