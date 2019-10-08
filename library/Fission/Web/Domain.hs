module Fission.Web.Domain
  ( API
  , server
  ) where

import RIO
import Data.Has

import           Servant

import           Fission.Web.Server
import           Fission.User        as User
import           Fission.IPFS.CID.Types

import qualified Network.AWS.Auth    as AWS
import qualified Fission.AWS.Types   as AWS
import qualified Fission.AWS.Route53 as Route53

type API = Capture "cid" CID
        :> Post    '[PlainText, OctetStream] NoContent

server :: Has AWS.AccessKey cfg
       => Has AWS.SecretKey cfg
       => Has AWS.ZoneId    cfg
       => Has AWS.Domain    cfg
       => HasLogFunc        cfg
       => User
       -> RIOServer         cfg API
server User { _userID } cid = do
  let subdomain = User.hashID _userID
  Route53.registerDomain subdomain cid
  logDebug "HERE"
  logDebug $ displayShow cid
  logDebug $ displayShow subdomain
  pure NoContent
