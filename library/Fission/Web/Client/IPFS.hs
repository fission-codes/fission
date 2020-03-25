module Fission.Web.Client.IPFS
  ( dagput
  , unpin
  , pin
  , upload
  , cids
  ) where

import           Fission.Prelude

import           Servant
import           Servant.Client

import qualified Network.IPFS.File.Types as File
import           Network.IPFS.CID.Types

import           Fission.Web.Routes (IPFSPrefix)

import           Fission.Web.Client

import qualified Fission.Web.IPFS        as IPFS
import qualified Fission.Web.IPFS.CID    as CID
import qualified Fission.Web.IPFS.Upload as Upload
import qualified Fission.Web.IPFS.Pin    as Pin
import qualified Fission.Web.IPFS.DAG    as DAG

type AuthedIPFS more = IPFS.Auth :> IPFSPrefix :> more

dagput :: File.Serialized -> ClientM CID
dagput = sigClient <| Proxy @(AuthedIPFS ("dag" :> DAG.API))

unpin :: CID -> ClientM NoContent
unpin = sigClient <| Proxy @(AuthedIPFS Pin.UnpinAPI)

pin :: CID -> ClientM NoContent
pin = sigClient <| Proxy @(AuthedIPFS Pin.PinAPI)

upload :: File.Serialized -> ClientM CID
upload = sigClient <| Proxy @(AuthedIPFS Upload.API)

cids :: ClientM [CID]
cids = sigClient' <| Proxy @(AuthedIPFS ("cids" :> CID.API))
