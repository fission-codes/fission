module Fission.Web.IPFS.Download
  ( API
  , get
  ) where

import RIO

import           Data.Has
import qualified Network.HTTP.Client as HTTP
import           Servant

import           Fission.Internal.Orphanage ()
import           Fission.Web.Server
import qualified Fission.Web.Error    as Web.Err
import           Fission.File.Types   as File
import qualified Fission.IPFS.Types   as IPFS
import           Fission.IPFS.CID.Types
import qualified Fission.IPFS.Client  as IPFS.Client

type API =  PathAPI
       :<|> QueryAPI

type PathAPI = Capture "cid" IPFS.CID
            :> Get '[OctetStream, PlainText] File.Serialized

type QueryAPI = QueryParam "cid" IPFS.CID
             :> Get '[OctetStream, PlainText] File.Serialized

get :: Has IPFS.URL      cfg
    => Has HTTP.Manager  cfg
    => HasLogFunc        cfg
    => RIOServer         cfg API
get = pathGet :<|> queryGet

queryGet :: Has IPFS.URL      cfg
         => Has HTTP.Manager  cfg
         => HasLogFunc        cfg
         => RIOServer         cfg QueryAPI
queryGet = \case
  Just cid -> pathGet cid
  Nothing  -> throwM err404

pathGet :: Has IPFS.URL      cfg
        => Has HTTP.Manager  cfg
        => HasLogFunc        cfg
        => RIOServer         cfg PathAPI
pathGet (CID hash) = IPFS.Client.run (IPFS.Client.cat hash) >>= Web.Err.ensure
