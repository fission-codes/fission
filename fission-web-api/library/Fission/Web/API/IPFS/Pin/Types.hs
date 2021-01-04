module Fission.Web.API.IPFS.Pin.Types (Pin) where

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.IPFS.Pin.Create.Types  as Pin
import qualified Fission.Web.API.IPFS.Pin.Destroy.Types as Pin

type Pin = Pin.Create :<|> Pin.Destroy
