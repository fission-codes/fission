module Fission.Web.IPFS
  ( API
  , Auth
  , AuthedAPI
  , PublicAPI
  , SimpleAPI
  , UnauthedAPI
  , authed
  , server
  ) where

import RIO
import RIO.Process (HasProcessContext)

import SuperRecord
import Database.Selda

import qualified Network.HTTP.Client as HTTP
import           Servant

import           Fission.IPFS.Types        as IPFS
import           Fission.User

import           Fission.Web.Server
import qualified Fission.Web.IPFS.CID           as CID
import qualified Fission.Web.IPFS.Upload        as Upload
import qualified Fission.Web.IPFS.Upload.Simple as Upload.Simple
import qualified Fission.Web.IPFS.Download      as Download
import qualified Fission.Web.IPFS.Pin           as Pin
import qualified Fission.Web.IPFS.DAG           as DAG

type API = AuthedAPI
      :<|> PublicAPI

type Auth = BasicAuth "registered users" User

type AuthedAPI = Auth :> UnauthedAPI

type UnauthedAPI = "cids" :> CID.API
              :<|> Upload.API
              :<|> Pin.API
              :<|> "dag" :> DAG.API

type SimpleAPI = "cids" :> CID.API
            :<|> Upload.Simple.API
            :<|> Pin.API
            :<|> "dag" :> DAG.API

type PublicAPI = Download.API

server :: HasLogFunc             (Rec cfg)
       => HasProcessContext      (Rec cfg)
       => MonadSelda        (RIO (Rec cfg))
       => Has "httpManager"           cfg HTTP.Manager
       => Has "ipfsURL"               cfg IPFS.URL
       => Has "ipfsPath"              cfg IPFS.BinPath
       => Has "ipfsTimeout"           cfg IPFS.Timeout
       => RIOServer    (Rec cfg) API
server = authed
    :<|> Download.get

authed :: HasLogFunc        (Rec cfg)
       => HasProcessContext (Rec cfg)
       => MonadSelda   (RIO (Rec cfg))
       => Has "httpManager"      cfg HTTP.Manager
       => Has "ipfsURL"          cfg IPFS.URL
       => Has "ipfsPath"         cfg IPFS.BinPath
       => Has "ipfsTimeout"      cfg IPFS.Timeout
       => RIOServer     (Rec cfg) AuthedAPI
authed usr = CID.allForUser usr
        :<|> Upload.add usr
        :<|> Pin.server usr
        :<|> DAG.put usr
