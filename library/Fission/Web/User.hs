module Fission.Web.User
  ( server
  , API
  , VerifyRoute
  ) where

import           Database.Selda
import           Servant
import           Network.AWS.Auth  as AWS

import           Fission.Prelude
import qualified Fission.AWS.Types as AWS
import           Fission.Web.Server
import qualified Fission.Web.User.Create as Create
import qualified Fission.Web.User.Verify as Verify
import qualified Fission.Web.User.Password.Reset as Reset
import qualified Fission.Web.Auth.Types  as Auth

import qualified Network.IPFS.Types as IPFS

type API = Create.API
      :<|> VerifyRoute
      :<|> ResetRoute

type VerifyRoute = "verify"
                   :> Auth.ExistingUser
                   :> Verify.API

type ResetRoute = "reset_password"
                  :> Auth.ExistingUser
                  :> Reset.API

server
  :: ( HasLogFunc          cfg
     , MonadSelda     (RIO cfg)
     , Has IPFS.Gateway    cfg
     , Has AWS.DomainName  cfg
     , Has AWS.AccessKey   cfg
     , Has AWS.SecretKey   cfg
     , Has AWS.ZoneID      cfg
     , Has AWS.Route53MockEnabled cfg
     )
  => RIOServer cfg API
server = Create.server
    :<|> const Verify.server
    :<|> Reset.server
