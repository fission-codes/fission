module Fission.Web.IPFS.Peer
  ( API
  , get
  ) where

import           Servant

import           Fission.Prelude
import           Fission.IPFS.Peer as IPFS.Peer
import           Fission.Web.Server
import qualified Fission.IPFS.Types as IPFS
import qualified Fission.Web.Error  as Web.Err

type API = Get '[JSON, PlainText, OctetStream] [IPFS.Peer]

-- | Get a list of valid IPFS addresses that a user could use to join our network
get
  :: ( Has IPFS.BinPath  cfg
     , Has IPFS.Timeout  cfg
     , HasProcessContext cfg
     , HasLogFunc        cfg
     )
  => RIOServer cfg API
get = getExternalAddress >>= \case
  Right peers -> return <| [IPFS.Peer.fission ] <> peers
  Left err    -> Web.Err.throw err
