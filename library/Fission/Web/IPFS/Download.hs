module Fission.Web.IPFS.Download
  ( API
  , get
  ) where

import           Servant

import           Fission.Prelude
import           Fission.Web.Server
import qualified Fission.Web.Error as Web.Err

import           Network.IPFS
import qualified Network.IPFS.Types      as IPFS
import qualified Network.IPFS.Get        as IPFS
import           Network.IPFS.File.Types as File

type API =  PathAPI
       :<|> QueryAPI

type PathAPI = Capture "cid" IPFS.CID
            :> Get '[OctetStream, PlainText] File.Serialized

type QueryAPI = QueryParam "cid" IPFS.CID
             :> Get '[OctetStream, PlainText] File.Serialized

get ::
  ( MonadLocalIPFS (RIO cfg)
  , HasLogFunc          cfg
  )
  => RIOServer          cfg API
get = pathGet :<|> queryGet

queryGet ::
  ( MonadLocalIPFS (RIO cfg) 
  , HasLogFunc          cfg
  )
  => RIOServer          cfg QueryAPI
queryGet = \case
  Just cid -> IPFS.getFile cid >>= Web.Err.ensure
  Nothing  -> throwM err404

pathGet ::
  ( MonadLocalIPFS (RIO cfg)
  , HasLogFunc          cfg
  )
  => RIOServer          cfg PathAPI
pathGet cid = IPFS.getFile cid >>= Web.Err.ensure
