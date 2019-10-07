module Fission.Web.Domain
  ( API
  , server
  ) where

import RIO

import Data.Has

import qualified Network.HTTP.Client as HTTP
import           Servant

import           Fission.Web.Server
import qualified Fission.Web.Error    as Web.Err
import           Fission.File.Types   as File
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS
import           Fission.User as User
import           Fission.User.CID.Mutation as UserCID
import           Fission.IPFS.CID.Types
import qualified Network.AWS.Auth       as AWS
import qualified Fission.AWS.Route53 as Route53

type API = Capture "cid" CID
        :> Post    '[PlainText, OctetStream] NoContent

server :: Has HTTP.Manager  cfg
       => Has AWS.AccessKey cfg
       => Has AWS.SecretKey cfg
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
