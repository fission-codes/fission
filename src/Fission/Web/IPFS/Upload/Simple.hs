module Fission.Web.IPFS.Upload.Simple
  ( API
  , add
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Servant

import           Fission.Web.Server
import qualified Fission.Web.Error    as Web.Err
import qualified Fission.File.Types   as File
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

add :: Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => RIOServer         cfg API
add = either Web.Err.throw pure <=< Storage.IPFS.addRaw . File.unserialize
