module Fission.Web.IPFS.Upload.Simple
  ( API
  , add
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Servant

import           Fission.Web.Server
import qualified Fission.File                  as File
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Storage.IPFS          as Storage.IPFS
import qualified Fission.Web.IPFS.Upload.Error as IPFS

type API = ReqBody '[PlainText, OctetStream] File.Serialized
        :> Post    '[PlainText, OctetStream] IPFS.CID

add :: Has IPFS.Path cfg
    => HasProcessContext cfg
    => HasLogFunc cfg
    => RIOServer cfg API
add raw = Storage.IPFS.addRaw (File.unserialize raw) >>= either IPFS.throwErr pure
