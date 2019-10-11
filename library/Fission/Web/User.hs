module Fission.Web.User
  ( server
  , API
  , VerifyRoute
  ) where

import RIO
import           RIO.Process (HasProcessContext)

import Data.Has
import Database.Selda

import Servant

import           Fission.Web.Server
import qualified Fission.Web.User.Create as Create
import qualified Fission.Web.User.Verify as Verify
import qualified Fission.Web.Auth.Types  as Auth
import qualified Fission.Web.Types       as Web
import           Fission.IPFS.Types      as IPFS

import           Network.AWS.Auth  as AWS
import qualified Fission.AWS.Types as AWS

type API = Create.API
      :<|> VerifyRoute

type VerifyRoute = "verify"
                   :> Auth.ExistingUser
                   :> Verify.API

server :: HasLogFunc        cfg
       => MonadSelda   (RIO cfg)
       => Has Web.Host      cfg
       => HasProcessContext cfg
       => Has IPFS.BinPath cfg
       => Has IPFS.Timeout cfg
       => Has AWS.DomainName    cfg
       => Has AWS.AccessKey  cfg
       => Has AWS.SecretKey  cfg
       => Has AWS.ZoneID     cfg
       => RIOServer         cfg API
server = Create.server
    :<|> const Verify.server
