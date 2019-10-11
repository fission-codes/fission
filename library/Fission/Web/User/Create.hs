{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.User.Create
  ( API
  , server
  ) where

import           RIO
import           RIO.Process (HasProcessContext)

import           Database.Selda as Selda

import           Servant
import           Data.Has
import qualified Fission.Config               as Config

import           Fission.Web.Server
import qualified Fission.Web.Types as Web

import qualified Fission.User                     as User
import qualified Fission.User.Provision.Types     as User
import qualified Fission.User.Registration.Types  as User

import           Fission.AWS
import qualified Fission.AWS.Types   as AWS
import           Fission.AWS.Route53

import           Fission.IPFS.Peer
import           Fission.IPFS.Types           as IPFS

import           Network.AWS.Auth    as AWS
import qualified Network.AWS.Route53 as Route53

import           Fission.Internal.UTF8
import           Fission.Security.Types (Secret (..))

type API = ReqBody '[JSON] User.Registration
        :> Post '[JSON] User.Provision


server :: HasLogFunc         cfg
       => Has Web.Host       cfg
       => HasProcessContext cfg
       => Has IPFS.BinPath cfg
       => Has IPFS.Timeout cfg
       => Has AWS.DomainName cfg
       => Has AWS.AccessKey  cfg
       => Has AWS.SecretKey  cfg
       => Has AWS.ZoneID     cfg
       => MonadSelda    (RIO cfg)
       => RIOServer          cfg API
server (User.Registration username password email) = do
  domain :: AWS.DomainName <- Config.get
  Web.Host url             <- Config.get
  ipfsPeers                <- getExternalAddress >>= \case
                                Right peers' ->
                                  pure peers'

                                Left err -> do
                                  logError $ displayShow err
                                  return []

  userID <- User.create username password email
  logInfo $ "Provisioned user: " <> displayShow userID

  let
    baseUrl    = username <> AWS.getDomainName domain
    dnsLinkUrl = "_dnslink." <> baseUrl
    dnsLinkTxt = "dnslink=/ipfs/" <> splashCID

  ensureContent $ registerDomain Route53.Cname baseUrl "ipfs.runfission.com"
  ensureContent $ registerDomain Route53.Txt dnsLinkUrl $ dnsLinkTxt `wrapIn` "\""

  return User.Provision
    { _url      = url
    , _username = username
    , _password = Secret password
    , _peers   = ipfsPeers
    }

splashCID :: Text
splashCID = "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
