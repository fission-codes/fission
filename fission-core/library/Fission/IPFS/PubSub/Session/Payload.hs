module Fission.IPFS.PubSub.Session.Payload
  ( toSecure
  , module Fission.IPFS.PubSub.Session.Payload.Types
  ) where

import           Crypto.Cipher.AES                         (AES256)
import           Crypto.Error
import           Crypto.Random.Types

import           Fission.Prelude

import           Fission.IPFS.PubSub.Session.Payload.Types
import qualified Fission.Key.Symmetric                     as Symmetric

toSecure ::
  ( MonadRandom m
  , MonadRaise  m
  , m `Raises` CryptoError
  , ToJSON msg
  )
  => Symmetric.Key AES256
  -> msg
  -> m (Payload msg)
toSecure aesKey msg = do
  Symmetric.genIV >>= \case
    Nothing ->
      undefined -- FIXME better error

    Just iv -> do
      secretMessage <- ensure $ Symmetric.encrypt aesKey iv msg
      return $ Payload {..}
