module Fission.Web.IPFS.Client
  ( API
  , Request (..)
  , SimpleAPI
  , request
  ) where

import RIO

import Servant
import Servant.Client

import qualified Fission.File.Types as File
import           Fission.IPFS.CID.Types

import qualified Fission.Web.Client as Client
import           Fission.Web.Routes (IPFSPrefix)

import qualified Fission.Web.IPFS               as IPFS
import qualified Fission.Web.IPFS.CID           as CID
import qualified Fission.Web.IPFS.Upload.Simple as Upload.Simple
import qualified Fission.Web.IPFS.Pin           as Pin
import qualified Fission.Web.IPFS.DAG           as DAG
import           Fission.IPFS.Peer.Types

type API = IPFSPrefix :> IPFS.Auth :> SimpleAPI

type SimpleAPI = "cids" :> CID.API
            :<|> Upload.Simple.API
            :<|> Pin.API
            :<|> "dag" :> DAG.API

data Request = Request
  { dagput :: File.Serialized -> ClientM CID
  , unpin  :: CID             -> ClientM NoContent
  , pin    :: CID             -> ClientM NoContent
  , upload :: File.Serialized -> ClientM CID
  , cids   :: ClientM [CID]
  -- FINISH OFF NOTE: I couldn't figure out how to make an auth request TO peers. IT may involve how were doing type coersion?
  , peers  :: ClientM [Peer]
  }

-- | Generate authenticate client functions
request :: BasicAuthData -> Request
request ba = Request {..}
  where
    cids :<|> peers :<|> upload :<|> (pin :<|> unpin) :<|> dagput = Client.withAuth ba (Proxy :: Proxy API)
