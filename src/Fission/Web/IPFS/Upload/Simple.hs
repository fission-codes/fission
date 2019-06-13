module Fission.Web.IPFS.Upload.Simple
  ( API
  , server
  ) where

import RIO

import Data.Has
import Servant

import Fission.Config
import Fission.Web.Server

import qualified Fission.File         as File
import qualified Fission.IPFS.Address as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS

type API = ReqBody '[OctetStream, PlainText] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.Address

server :: Has IPFSPath cfg => RIOServer cfg API
server = Storage.IPFS.add . File.unserialize
