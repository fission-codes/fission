module Fission.Web.IPFS.Upload
  ( API
  , add
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Database.Selda

import qualified Network.HTTP.Client as HTTP
import           Servant

-- import           Fission.Internal.Orphanage ()
import qualified Fission.Web.IPFS.Upload.Multipart as Multipart
import qualified Fission.Web.IPFS.Upload.Simple    as Simple
import qualified Fission.IPFS.Types   as IPFS
import           Fission.Web.Server
import           Fission.User

type API = Simple.API :<|> Multipart.API

add :: Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => Has HTTP.Manager  cfg
    => Has IPFS.URL      cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => MonadSelda   (RIO cfg)
    => User
    -> RIOServer         cfg API
add usr = Simple.add usr :<|> Multipart.add usr
