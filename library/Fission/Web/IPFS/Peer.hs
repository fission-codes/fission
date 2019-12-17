module Fission.Web.IPFS.Peer
  ( API
  , get
  ) where

import           Servant

import           Fission.Prelude
import qualified Fission.Web.Error  as Web.Err
import qualified Fission.Config     as Config

import           Network.IPFS
import           Network.IPFS.Peer  as IPFS.Peer
import qualified Network.IPFS.Types as IPFS

type API = Get '[JSON, PlainText, OctetStream] [IPFS.Peer]

-- | Get a list of valid IPFS addresses that a user could use to join our network
get ::
  ( MonadReader   cfg m
  , Has IPFS.Peer cfg
  , MonadLocalIPFS    m
  , MonadLogger       m
  , MonadThrow        m
  )
  => ServerT API m
get = do
  remotePeer <- Config.get
  getExternalAddress >>= \case
    Right peers -> return (remotePeer : peers)
    Left err    -> Web.Err.throw err
