module Fission.Key.Error (Error(..)) where

import Fission.Prelude
import Crypto.Error

data Error
  = DoesNotExist
  | AlreadyExists
  | ParseError CryptoError
  deriving ( Exception
            , Eq
            , Generic
            )

instance Show Error where
  show = \case
    DoesNotExist -> "~/.ssh/fission does not exist"
    AlreadyExists -> "~/.ssh/fission already exists"
    ParseError err -> "Parse error" <> show err
