module Fission.IPFS.PubSub.Publish
  ( sendClear
  , sendSecure
  ) where

import           Crypto.Error
import           Crypto.Random.Types

import qualified RIO.Text                              as Text

import           Network.IPFS.Local.Class              as IPFS
import qualified Network.IPFS.Process.Error            as IPFS.Process

import           Fission.Prelude

import qualified Fission.IPFS.PubSub.Session.Key.Types as Session
import qualified Fission.IPFS.PubSub.Session.Payload   as Payload
import           Fission.IPFS.PubSub.Topic

sendClear ::
  ( MonadLocalIPFS m
  , MonadLogger    m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  , ToJSON  msg
  , Display msg
  )
  => Topic
  -> msg
  -> m ()
sendClear topic msg = do
  logDebug $ "Broadcasting in cleartext over IFPS PubSub: " <> textDisplay msg
  void . ensureM $ IPFS.runLocal ["pubsub", "pub", Text.unpack $ textDisplay topic] (encode msg)

sendSecure ::
  ( MonadLocalIPFS m
  , MonadLogger    m
  , MonadRandom    m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  , m `Raises` CryptoError
  , ToJSON msg
  )
  => Topic
  -> Session.Key
  -> msg
  -> m ()
sendSecure topic (Session.Key aesKey) msg = do
  encrypted <- Payload.toSecure aesKey msg
  sendClear topic encrypted
