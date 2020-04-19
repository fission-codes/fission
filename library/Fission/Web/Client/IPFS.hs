module Fission.Web.Client.IPFS
  ( dagput
  , Unpin
  , unpin
  , Pin
  , pin
  , upload
  , cids
  ) where

import           Fission.Prelude

import           Servant
-- import           Servant.Client

-- import qualified Network.IPFS.File.Types as File
-- import           Network.IPFS.CID.Types

import           Fission.Web.Routes (IPFSPrefix)

-- import           Fission.Web.Client

import qualified Fission.Web.IPFS        as IPFS
import qualified Fission.Web.IPFS.CID    as CID
import qualified Fission.Web.IPFS.Upload as Upload
import qualified Fission.Web.IPFS.Pin    as Pin
import qualified Fission.Web.IPFS.DAG    as DAG

type AuthedIPFS more
  = IPFS.Auth
    :> IPFSPrefix
    :> more

dagput :: Proxy (AuthedIPFS ("dag" :> DAG.API))
dagput = Proxy

-- dagput :: MonadAuthedEndpoint m => File.Serialized -> m CID
-- dagput = withUCAN (Proxy @(AuthedIPFS ("dag" :> DAG.API)))

-- dagput :: File.Serialized -> ClientM CID
-- dagput = sigClient $ Proxy @(AuthedIPFS ("dag" :> DAG.API))

-- unpin :: MonadAuthedEndpoint m => CID -> m NoContent
-- unpin = withUCAN (Proxy @(AuthedIPFS Pin.UnpinAPI))

type Unpin = AuthedIPFS Pin.UnpinAPI

unpin :: Proxy Unpin
unpin = Proxy

-- unpin :: CID -> ClientM NoContent
-- unpin = sigClient $ Proxy @(AuthedIPFS Pin.UnpinAPI)

type Pin = AuthedIPFS Pin.PinAPI

pin :: Proxy Pin
pin = Proxy

-- pin :: CID -> ClientM NoContent
-- pin = sigClient $ Proxy @(AuthedIPFS Pin.PinAPI)

-- FIXME can probably delete
upload :: Proxy (AuthedIPFS Upload.API)
upload = Proxy

-- upload :: File.Serialized -> ClientM CID
-- upload = sigClient $ Proxy @(AuthedIPFS Upload.API)

cids :: Proxy (AuthedIPFS ("cids" :> CID.API))
cids = Proxy

-- cids :: ClientM [CID]
-- cids = sigClient' $ Proxy @(AuthedIPFS ("cids" :> CID.API))
