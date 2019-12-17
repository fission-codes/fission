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

import           Database.Esqueleto
import           Servant

import           Network.IPFS
import           Network.IPFS.Types as IPFS

import           Fission.Models
import           Fission.Prelude

import qualified Fission.Web.IPFS.CID      as CID
import qualified Fission.Web.IPFS.Upload   as Upload
import qualified Fission.Web.IPFS.Download as Download
import qualified Fission.Web.IPFS.Pin      as Pin
import qualified Fission.Web.IPFS.DAG      as DAG
import qualified Fission.Web.IPFS.Peer     as Peer

type API = AuthedAPI
      :<|> PublicAPI

type Auth = BasicAuth "registered users" (Entity User)

type AuthedAPI = Auth :> UnauthedAPI

type UnauthedAPI = "cids" :> CID.API
              :<|> Upload.API
              :<|> Pin.API
              :<|> "dag" :> DAG.API

type PublicAPI = "peers" :> Peer.API
            :<|> Download.API

server ::
  ( MonadReader   cfg m
  , Has IPFS.Peer cfg
  , MonadRemoteIPFS   m
  , MonadLocalIPFS    m
  , MonadLogger       m
  , MonadThrow        m
  , MonadTime         m
  , MonadDB           m
  )
  => ServerT API m
server = authed :<|> public

authed ::
  ( MonadDB         m
  , MonadRemoteIPFS m
  , MonadLocalIPFS  m
  , MonadLogger     m
  , MonadThrow      m
  , MonadTime       m
  )
  => ServerT AuthedAPI m
authed usr = CID.allForUser usr
        :<|> Upload.add usr
        :<|> Pin.server usr
        :<|> DAG.put usr

public ::
  ( MonadReader   cfg m
  , Has IPFS.Peer cfg
  , MonadLocalIPFS    m
  , MonadLogger       m
  , MonadThrow        m
  )
  => ServerT PublicAPI m
public = Peer.get
    :<|> Download.get
