module Fission.Web.IPFS.Peer
  ( API
  , get
  ) where

import           Servant

import           Fission.Prelude
import           Fission.Web.Server
import qualified Fission.Web.Error  as Web.Err
import qualified Fission.Config     as Config

import           Network.IPFS.Local.Class
import           Network.IPFS.Peer as IPFS.Peer
import qualified Network.IPFS.Types as IPFS

type API = Get '[JSON, PlainText, OctetStream] [IPFS.Peer]

-- | Get a list of valid IPFS addresses that a user could use to join our network
get ::
  ( MonadLocalIPFS (RIO cfg)
  , Has IPFS.Peer  cfg
  , HasLogFunc        cfg
  )
  => RIOServer cfg API
get = do
  remotePeer <- Config.get
  getExternalAddress >>= \case
    Right peers -> return (remotePeer : peers)
    Left err    -> Web.Err.throw err
