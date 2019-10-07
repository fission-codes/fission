module Fission.Web.Domain
  ( API
  , server
  ) where

import           RIO

import           Database.Selda as Selda

import           Servant
import           Data.Has
import qualified Fission.Config as Config

import           Fission.Web.Server
import qualified Fission.Web.Types       as Web
import qualified Fission.AWS.Types       as AWS

import qualified Fission.Random as Random

import qualified Fission.User                 as User

import           Fission.Security.Types (Secret (..))

import qualified Fission.AWS.Types       as AWS
import qualified Fission.AWS.Route53 as Route53

type API =  Post '[PlainText, OctetStream] NoContent

server :: HasLogFunc       cfg
          => Has Web.Host    cfg
          => Has AWS.AccessKey    cfg
          => Has AWS.SecretKey    cfg
          => MonadSelda (RIO cfg)
          => RIOServer       cfg API
server = do
  AWS.AccessKey accessKey <- Config.get
  AWS.SecretKey secretKey <- Config.get
  logDebug "HERE"
  logDebug $ displayShow accessKey
  logDebug $ displayShow secretKey
  pure NoContent