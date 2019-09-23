module Fission.Web.IPFS.Download
  ( API
  , get
  ) where

import RIO
import RIO.Process (HasProcessContext)

import SuperRecord hiding (get)
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

get :: Has "ipfsPath"         cfg IPFS.BinPath
    => Has "ipfsTimeout"      cfg Natural
    => HasProcessContext (Rec cfg)
    => HasLogFunc        (Rec cfg)
    => RIOServer         (Rec cfg) API
get = pathGet :<|> queryGet

queryGet :: Has "ipfsPath"         cfg IPFS.BinPath
         => Has "ipfsTimeout"      cfg Natural
         => HasProcessContext (Rec cfg)
         => HasLogFunc        (Rec cfg)
         => RIOServer         (Rec cfg) QueryAPI
queryGet = \case
  Just cid -> Storage.IPFS.get cid >>= Web.Err.ensure
  Nothing  -> throwM err404

pathGet :: Has "ipfsPath"         cfg IPFS.BinPath
        => Has "ipfsTimeout"      cfg Natural
        => HasProcessContext (Rec cfg)
        => HasLogFunc        (Rec cfg)
        => RIOServer         (Rec cfg) PathAPI
pathGet cid = Storage.IPFS.get cid >>= Web.Err.ensure
