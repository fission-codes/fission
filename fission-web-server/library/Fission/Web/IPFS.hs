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

import           Network.IPFS
import           Servant

import           Fission.IPFS.Linked
import           Fission.Prelude

import qualified Fission.LoosePin as LoosePin
import qualified Fission.Web.Auth.Types as Auth

import qualified Fission.Web.IPFS.CID      as CID
import qualified Fission.Web.IPFS.Upload   as Upload
import qualified Fission.Web.IPFS.Download as Download
import qualified Fission.Web.IPFS.Pin      as Pin
import qualified Fission.Web.IPFS.DAG      as DAG
import qualified Fission.Web.IPFS.Peer     as Peer

type API
  =    AuthedAPI
  :<|> PublicAPI

type Auth
  = Auth.HigherOrder

type AuthedAPI
  = Auth
    :> UnauthedAPI

type UnauthedAPI
  = "cids"
    :> CID.API
      :<|> Upload.API
      :<|> Pin.API
      :<|> "dag"
           :> DAG.API

type PublicAPI
  = "peers"
    :> Peer.API :<|> Download.API

server ::
  ( MonadRemoteIPFS      m
  , MonadLinkedIPFS      m
  , MonadLocalIPFS       m
  , MonadLogger          m
  , MonadThrow           m
  , MonadTime            m
  , MonadDB            t m
  , LoosePin.Creator   t
  , LoosePin.Retriever t
  , LoosePin.Destroyer t
  )
  => ServerT API m
server = authed :<|> public

authed ::
  ( MonadRemoteIPFS      m
  , MonadLocalIPFS       m
  , MonadLogger          m
  , MonadThrow           m
  , MonadTime            m
  , MonadDB            t m
  , LoosePin.Creator   t
  , LoosePin.Retriever t
  , LoosePin.Destroyer t
  )
  => ServerT AuthedAPI m
authed auth = CID.allForUser auth
         :<|> Upload.add     auth
         :<|> Pin.server     auth
         :<|> DAG.put        auth

public ::
  ( MonadLinkedIPFS m
  , MonadLocalIPFS  m
  , MonadLogger     m
  , MonadThrow      m
  )
  => ServerT PublicAPI m
public = Peer.get
    :<|> Download.get
