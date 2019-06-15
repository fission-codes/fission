module Fission.Web.IPFS.Upload.Simple
  ( API
  , add
  ) where

import RIO

import Data.Has
import Servant

import           Fission.Web.Server
import qualified Fission.File         as File
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.Address

add :: Has IPFS.Path cfg => RIOServer cfg API
add = Storage.IPFS.add . File.unserialize
