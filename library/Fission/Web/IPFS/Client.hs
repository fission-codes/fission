module Fission.Web.IPFS.Client where
  -- ( pin
  -- , unpin
  -- ) where

import RIO

import Servant
import Servant.Client

import           Fission.IPFS.CID.Types
import qualified Fission.Web.IPFS     as IPFS
import qualified Fission.Web.IPFS.Pin as IPFS

type Prefix   = "ipfs" :> IPFS.Auth

type PinAPI   = Prefix :> IPFS.PinAPI
type UnpinAPI = Prefix :> IPFS.UnpinAPI

-- pin :: BasicAuthData -> CID -> ClientM NoContent
-- pin :: CID -> ClientM NoContent
-- pin = client (Proxy :: Proxy PinAPI)

-- -- unpin :: BasicAuthData -> CID -> ClientM NoContent
-- -- unpin :: CID -> ClientM NoContent
-- unpin = client (Proxy :: Proxy UnpinAPI)
