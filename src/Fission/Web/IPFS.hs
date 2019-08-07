module Fission.Web.IPFS
  ( API
  , AuthedAPI
  , PublicAPI
  , authed
  , server
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Database.Selda
import Servant

import           Fission.IPFS.Types        as IPFS
import           Fission.User

import           Fission.Web.Server
import qualified Fission.Web.IPFS.CID      as CID
import qualified Fission.Web.IPFS.Upload   as Upload
import qualified Fission.Web.IPFS.Download as Download
import qualified Fission.Web.IPFS.Pin      as Pin

type API = PublicAPI :<|> AuthedAPI

type AuthedAPI = BasicAuth "registered users" User
                 :> AuthedAPI'

type AuthedAPI' = "cids" :> CID.API
             :<|> Upload.API
             :<|> Pin.API

type PublicAPI = Download.API

server :: HasLogFunc        cfg
       => HasProcessContext cfg
       => MonadSelda   (RIO cfg)
       => Has IPFS.BinPath  cfg
       => Has IPFS.Timeout  cfg
       -- => User
       => RIOServer         cfg API
server = Download.get
        :<|> authed

authed :: HasLogFunc        cfg
       => HasProcessContext cfg
       => MonadSelda   (RIO cfg)
       => Has IPFS.BinPath  cfg
       => Has IPFS.Timeout  cfg
       => RIOServer         cfg AuthedAPI
authed usr = CID.allForUser usr
        :<|> Upload.add usr
        :<|> Pin.server usr
