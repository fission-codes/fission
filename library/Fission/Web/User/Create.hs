{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.User.Create
  ( API
  , server
  ) where

import           Flow
import           RIO

import           Database.Selda as Selda

import           Servant
import           Data.Has
import qualified Fission.Config as Config

import           Fission.Web.Server

import qualified Fission.User                     as User
import qualified Fission.User.Registration.Types  as User

import           Fission.AWS
import qualified Fission.AWS.Types   as AWS
import           Fission.AWS.Route53

import           Network.AWS.Auth    as AWS
import qualified Network.AWS.Route53 as Route53

import           Fission.Internal.UTF8

type API = ReqBody '[JSON] User.Registration
        :> Post '[JSON] ()

server
  :: ( HasLogFunc         cfg
     , Has AWS.DomainName cfg
     , Has AWS.AccessKey  cfg
     , Has AWS.SecretKey  cfg
     , Has AWS.ZoneID     cfg
     , MonadSelda    (RIO cfg)
     )
  => RIOServer cfg API
server (User.Registration username password email) = do
  domain :: AWS.DomainName <- Config.get

  userID <- User.create username password email
  logInfo <| "Provisioned user: " <> displayShow userID

  let
    baseUrl    = username <> AWS.getDomainName domain
    dnsLinkUrl = "_dnslink." <> baseUrl
    dnsLinkTxt = "dnslink=/ipfs/" <> splashCID

  "ipfs.runfission.com"
    |> registerDomain Route53.Cname baseUrl
    |> ensureContent

  dnsLinkTxt
    |> wrapIn' "\""
    |> registerDomain Route53.Txt dnsLinkUrl
    |> ensureContent

  return ()

wrapIn' :: Text -> Text -> Text
wrapIn' a b = wrapIn b a

splashCID :: Text
splashCID = "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
