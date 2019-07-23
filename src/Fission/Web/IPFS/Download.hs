module Fission.Web.IPFS.Download
  ( API
  , get
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Servant

import           Fission.Web.Server
import qualified Fission.Web.Error    as Web.Err
import           Fission.File.Types   as File
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS

type API =  PathAPI
       :<|> QueryAPI

type PathAPI = Capture "cid" IPFS.CID
            :> Get '[OctetStream, PlainText] File.Serialized

type QueryAPI = QueryParam "cid" IPFS.CID
             :> Get '[OctetStream, PlainText] File.Serialized

get :: Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => RIOServer         cfg API
get = pathGet :<|> queryGet

queryGet :: Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => RIOServer         cfg QueryAPI
queryGet = \case
  Just cid -> Storage.IPFS.get cid >>= Web.Err.ensure
  Nothing  -> throwM err404

pathGet :: Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => RIOServer         cfg PathAPI
pathGet cid = Storage.IPFS.get cid >>= Web.Err.ensure
