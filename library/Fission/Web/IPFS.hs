module Fission.Web.IPFS
  ( API
  , Auth
  , AuthedAPI
  , PublicAPI
  , UnauthedAPI
  , authed
  , public
  , server
  ) where

import           Database.Selda

import           Servant

import           Fission.Prelude
import           Fission.User

import           Network.IPFS
import           Network.IPFS.Types as IPFS

import           Fission.Web.Server
import qualified Fission.Web.IPFS.CID      as CID
import qualified Fission.Web.IPFS.Upload   as Upload
import qualified Fission.Web.IPFS.Download as Download
import qualified Fission.Web.IPFS.Pin      as Pin
import qualified Fission.Web.IPFS.DAG      as DAG
import qualified Fission.Web.IPFS.Peer     as Peer

type API = AuthedAPI
      :<|> PublicAPI

type Auth = BasicAuth "registered users" User

type AuthedAPI = Auth :> UnauthedAPI

type UnauthedAPI = "cids" :> CID.API
              :<|> Upload.API
              :<|> Pin.API
              :<|> "dag" :> DAG.API

type PublicAPI = "peers" :> Peer.API
            :<|> Download.API

server ::
  ( MonadSelda      (RIO cfg)
  , MonadRemoteIPFS (RIO cfg)
  , MonadLocalIPFS  (RIO cfg)
  , HasLogFunc           cfg
  , Has IPFS.Peer     cfg
  )
  => RIOServer           cfg API
server = authed :<|> public

authed ::
  ( MonadSelda      (RIO cfg)
  , MonadRemoteIPFS (RIO cfg)
  , MonadLocalIPFS  (RIO cfg)
  , HasLogFunc           cfg
  )
  => RIOServer           cfg AuthedAPI
authed usr = CID.allForUser usr
        :<|> Upload.add usr
        :<|> Pin.server usr
        :<|> DAG.put usr

public ::
  ( MonadLocalIPFS (RIO cfg)
  , Has IPFS.Peer     cfg
  , HasLogFunc          cfg
  )
  => RIOServer          cfg PublicAPI
public = Peer.get
    :<|> Download.get
