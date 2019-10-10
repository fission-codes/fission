module Fission.Web.DNS
  ( API
  , server
  ) where

import RIO

import Data.Has

import           Servant

import Fission.Internal.UTF8

import qualified Fission.Config as Config
import           Fission.Web.Server
import           Fission.User        as User
import           Fission.IPFS.CID.Types

import qualified Network.AWS.Auth    as AWS
import qualified Fission.AWS.Types   as AWS
import qualified Fission.AWS.Route53 as Route53
import qualified Network.AWS.Route53 as Route53

type API = Capture "cid" CID
        :> PutAccepted '[PlainText, OctetStream] AWS.DomainName

server :: HasLogFunc         cfg
       => Has AWS.AccessKey  cfg
       => Has AWS.SecretKey  cfg
       => Has AWS.ZoneId     cfg
       => Has AWS.DomainName cfg
       => User
       -> RIOServer         cfg API
server User { _userID } cid = do 
  domain :: AWS.DomainName <- Config.get
  let
    username = User.hashID _userID
    baseUrl = username <> AWS.getDomainName domain
    dnslinkUrl = "_dnslink." <> baseUrl
    dnslink = "dnslink=/ipfs/" <> unaddress cid

  res1 <- Route53.registerDomain Route53.Cname baseUrl "ipfs.runfission.com"
  res2 <- Route53.registerDomain Route53.Txt dnslinkUrl $ dnslink `wrapIn` "\""

  case res1 of
    Just err -> throwM err
    Nothing -> case res2 of
      Just err -> throwM err
      Nothing -> return $ AWS.DomainName baseUrl
