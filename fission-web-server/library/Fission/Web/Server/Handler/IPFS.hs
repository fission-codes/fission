module Fission.Web.Server.Handler.IPFS (handler) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.Types               as API

import           Fission.Web.Server.IPFS.Linked
import qualified Fission.Web.Server.LoosePin              as LoosePin
import           Fission.Web.Server.MonadDB

import qualified Fission.Web.Server.Handler.IPFS.CID      as CID
import qualified Fission.Web.Server.Handler.IPFS.DAG      as DAG
import qualified Fission.Web.Server.Handler.IPFS.Download as Download
import qualified Fission.Web.Server.Handler.IPFS.Peer     as Peer
import qualified Fission.Web.Server.Handler.IPFS.Pin      as Pin
import qualified Fission.Web.Server.Handler.IPFS.Upload   as Upload

handler ::
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
  => ServerT API.IPFS m
handler = CID.allForUser
     :<|> DAG.put
     :<|> Peer.get
     :<|> Upload.add
     :<|> Pin.handler
     :<|> Download.get
