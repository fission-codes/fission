module Fission.Web.IPFS.Upload
  ( API
  , server
  ) where

import RIO

import Data.Has
import Servant

import           Fission.Config
import qualified Fission.Web.IPFS.Upload.Multipart as Multipart
import qualified Fission.Web.IPFS.Upload.Simple    as Simple
import           Fission.Web.Server

type API = Simple.API :<|> Multipart.API

server :: (Has IPFSPath cfg, HasLogFunc cfg) => RIOServer cfg API
server = Simple.server :<|> Multipart.server
