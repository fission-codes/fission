{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.User.Create
  ( API
  , server
  ) where

import           RIO

import           Database.Selda as Selda

import           Servant
import           Data.Has
import qualified Fission.Config as Config

import           Fission.Web.Server
import qualified Fission.Web.Types as Web

import qualified Fission.User                     as User
import qualified Fission.User.Provision.Types     as User
import qualified Fission.User.Registration.Types  as User

import           Fission.AWS
import qualified Fission.AWS.Types   as AWS
import           Fission.AWS.Route53

import           Network.AWS.Auth as AWS
import qualified Network.AWS.Route53 as Route53

import           Fission.Internal.UTF8
import           Fission.Security.Types (Secret (..))
  
type API = ReqBody '[JSON] User.Registration 
        :> Post '[JSON] User.Provision


server :: HasLogFunc      cfg
       => Has Web.Host    cfg
       => Has AWS.DomainName    cfg
       => Has AWS.AccessKey  cfg
       => Has AWS.SecretKey  cfg
       => Has AWS.ZoneID     cfg
       => MonadSelda (RIO cfg)
       => RIOServer       cfg API
server (User.Registration username password email) = do
  Web.Host url <- Config.get
  domain :: AWS.DomainName <- Config.get
  userID       <- User.create username password email
  logInfo $ "Provisioned user: " <> displayShow userID

  let
    baseUrl    = username <> AWS.getDomainName domain
    dnslinkUrl = "_dnslink." <> baseUrl
    dnslink    = "dnslink=/ipfs/" <> splashCID

  ensureContent $ registerDomain Route53.Cname baseUrl "ipfs.runfission.com"
  ensureContent $ registerDomain Route53.Txt dnslinkUrl $ dnslink `wrapIn` "\""


  return User.Provision
    { _url      = url
    , _username = username
    , _password = Secret password
    }

splashCID :: Text
splashCID = "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
