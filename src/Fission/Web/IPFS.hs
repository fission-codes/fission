module Fission.Web.IPFS
  ( API
  , server
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Servant
import Data.Has

import           Fission.IPFS.Types as IPFS
import           Fission.Web.Server
import qualified Fission.Web.IPFS.Upload as Upload
import qualified Fission.Web.IPFS.Pin    as Pin

type API = Upload.API
      :<|> Pin.API

server :: HasLogFunc        cfg
       => HasProcessContext cfg
       => Has IPFS.BinPath  cfg
       => RIOServer         cfg API
server = Upload.add :<|> Pin.server
