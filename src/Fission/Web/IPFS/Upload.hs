module Fission.Web.IPFS.Upload
  ( API
  , add
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Servant

import qualified Fission.Web.IPFS.Upload.Multipart as Multipart
import qualified Fission.Web.IPFS.Upload.Simple    as Simple
import qualified Fission.IPFS.Types   as IPFS
import           Fission.Web.Server
import           Fission.User

type API = Simple.API :<|> Multipart.API

add :: Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => User
    -> RIOServer         cfg API
add usr = Simple.add usr :<|> Multipart.add usr
