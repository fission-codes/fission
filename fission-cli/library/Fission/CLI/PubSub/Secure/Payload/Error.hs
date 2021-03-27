module Fission.CLI.PubSub.Secure.Payload.Error (Error (..)) where

import           Crypto.Error
import qualified Crypto.PubKey.RSA as RSA

import qualified RIO.Text          as Text

import           Fission.Prelude

data Error
  = CannotDecrypt       CryptoError
  | CannotDecryptRSA    RSA.Error
  | UnableToDeserialize String
  deriving (Eq, Show, Exception)

instance Display Error where
  textDisplay = Text.pack . show
