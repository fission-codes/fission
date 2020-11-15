module Fission.PubSub.DM
  ( getDID
  , receive
  , send
  , module Fission.PubSub.DM.Class
  ) where

import           Data.ByteArray          as ByteArray

import           Crypto.Error
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP  as RSA.OAEP

import           Fission.Prelude

import           Fission.PubSub
import           Fission.PubSub.DM.Class

getDID :: MonadPubSubDM m => m DID
getDID = do
  sk <- getDMKey
  reurn $ DID Key (Ed25519PublicKey $ toPublic sk)

send ::
  ( MonadPubSubSecure m
  , MonadRaise        m
  , m `Raises` String
  , ToJSON msg
  )
  => msg
  -> m ()
send = do
  payload <- listen
  ensure $ fromPayload payload

receive ::
  ( MonadPubSubDM m
  , MonadRaise    m
  , m `Raises` String
  , FromJSON msg
  )
  => m msg
receive = do
  secureDM <- listen
  sk       <- getDMKey

  undefined
  -- FIXME just before we break out into modules
--   RSA.OAEP.decryptSafer oaepParams sk secureDM >>= \case
--     Left err -> do
--       -- logDebug $ "Unable to decrypt message via RSA: " <> decodeUtf8Lenient secretMsg
--       raise err
--
--     Right clearBS ->
--       case eitherDecodeStrict clearBS of
--         -- FIXME better "can't decode JSON" error
--         Left err -> do
--           -- logDebug $ "Unable to decode RSA-decrypted message. Raw = " <> decodeUtf8Lenient clearBS
--           raise err
--
--         Right dm ->
--           return dm

oaepParams ::
  ( ByteArray       output
  , ByteArrayAccess seed
  )
  => RSA.OAEP.OAEPParams SHA256 seed output
oaepParams = RSA.OAEP.defaultOAEPParams SHA256
