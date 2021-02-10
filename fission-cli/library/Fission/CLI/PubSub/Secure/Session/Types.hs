module Fission.CLI.PubSub.Secure.Session.Types (Session (..)) where

import           Crypto.Cipher.AES                                 (AES256)
import qualified Crypto.PubKey.RSA                                 as RSA

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types                       as Symmetric
import qualified Fission.Web.Auth.Token.Bearer.Types               as Bearer

import           Fission.CLI.PubSub.Secure.Payload.Family
import           Fission.CLI.PubSub.Secure.Session.Handshake.Types

data Session = Session
  { bearerToken :: Bearer.Token
  , sessionKey  :: Symmetric.Key AES256
  }
  deriving Eq

type instance SecurePayload (RSA.PublicKey, RSA.PrivateKey) Session = Handshake
