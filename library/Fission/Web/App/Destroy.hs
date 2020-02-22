module Fission.Web.App.Destroy
  ( API
  -- , server
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.IPFS.DNSLink.Class as DNSLink
import qualified Fission.URL.Types          as URL
import           Fission.Web.Error          as Web.Err
import           Fission.User.Username.Types

type API
  =  Capture "appId" AppId
  -- :> Deleted '[JSON] NoResponse
