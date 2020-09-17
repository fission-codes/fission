module Fission.Web.User.DataRoot
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Authorization

import           Fission.Web.Error as Web.Error
import qualified Fission.Web.Auth.Types as Auth

import qualified Fission.User as User

import           Network.IPFS.CID.Types

import qualified Fission.Web.User.DataRoot.Update as Update
import qualified Fission.Web.User.DataRoot.Get as Get

type API = UpdateAPI :<|> Get.API

type UpdateAPI = 
  Auth.HigherOrder
  :> Update.API

server = Update.server :<|> Get.server