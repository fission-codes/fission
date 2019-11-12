module Fission.Web.DNS
  ( API
  , server
  ) where

import           Flow
import           RIO

import           Data.Has
import           Servant

import qualified Network.AWS.Auth    as AWS
import qualified Network.AWS.Route53 as Route53

import           Fission.AWS
import           Fission.AWS.Route53
import qualified Fission.AWS.Types   as AWS

import qualified Fission.Config as Config
import           Fission.Internal.UTF8

import           Fission.IPFS.CID.Types
import           Fission.User        as User

import           Fission.Web.Server

type API = Capture "cid" CID
        :> PutAccepted '[PlainText, OctetStream] AWS.DomainName

server
  :: ( HasLogFunc         cfg
     , Has AWS.AccessKey  cfg
     , Has AWS.SecretKey  cfg
     , Has AWS.ZoneID     cfg
     , Has AWS.DomainName cfg
     )
  => User
  -> RIOServer cfg API
server User { username } (CID hash) = do
  domain :: AWS.DomainName <- Config.get

  let
    baseUrl    = username <> AWS.getDomainName domain
    dnslinkUrl = "_dnslink." <> baseUrl
    dnslink    = "dnslink=/ipfs/" <> hash

  "ipfs.runfission.com"
    |> registerDomain Route53.Cname baseUrl
    |> ensureContent

  dnslink
    |> wrapIn' "\""
    |> registerDomain Route53.Txt dnslinkUrl
    |> ensureContent

  return <| AWS.DomainName baseUrl

wrapIn' :: Text -> Text -> Text
wrapIn' a b = wrapIn b a
