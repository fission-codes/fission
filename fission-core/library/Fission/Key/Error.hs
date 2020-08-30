module Fission.Key.Error (Error(..)) where

import           Crypto.Error
import qualified RIO.Text        as Text

import           Fission.Prelude

data Error
  = DoesNotExist
  | AlreadyExists
  | ParseError CryptoError
  deriving (Exception, Eq)

instance Show Error where
  show = \case
    DoesNotExist -> "~/.ssh/fission does not exist"
    AlreadyExists -> "~/.ssh/fission already exists"
    ParseError err -> "Parse error" <> show err

instance Display Error where
  textDisplay = Text.pack . show
