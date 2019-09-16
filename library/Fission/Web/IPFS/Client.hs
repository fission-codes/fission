module Fission.Web.IPFS.Client
  ( Request (..)
  , request
  ) where

import RIO

import Servant
import Servant.Client

import qualified Fission.File.Types as File
import           Fission.IPFS.CID.Types

import qualified Fission.Web.Client as Client
import qualified Fission.Web.IPFS   as IPFS
import           Fission.Web.Routes (IPFSPrefix)

type API = IPFSPrefix :> IPFS.Auth :> IPFS.SimpleAPI

data Request = Request
  { unpin  :: CID             -> ClientM NoContent
  , pin    :: CID             -> ClientM NoContent
  , upload :: File.Serialized -> ClientM CID
  , cids   :: ClientM [CID]
  }

-- | Generate authenticate client functions
request :: BasicAuthData -> Request
request ba = Request {..}
  where
    cids :<|> upload :<|> pin :<|> unpin = Client.withAuth ba (Proxy :: Proxy API)
