module Fission.Web.Server.Handler.IPFS
  ( authed
  , public
  , handler
  ) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import qualified Fission.Web.Server.Auth.Types    as Auth
import qualified Fission.Web.Server.IPFS.CID      as CID
import qualified Fission.Web.Server.IPFS.DAG      as DAG
import qualified Fission.Web.Server.IPFS.Download as Download
import           Fission.Web.Server.IPFS.Linked
import qualified Fission.Web.Server.IPFS.Peer     as Peer
import qualified Fission.Web.Server.IPFS.Pin      as Pin
import qualified Fission.Web.Server.IPFS.Upload   as Upload
import qualified Fission.Web.Server.LoosePin      as LoosePin

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
  => ServerT API m
handler = authed :<|> public

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
