module Fission.Web.DNS
  ( API
  , server
  ) where


import qualified Network.AWS.Auth    as AWS
import           Servant

import           Fission.Prelude

import           Fission.AWS.Route53
import qualified Fission.AWS.Types   as AWS

import qualified Fission.IPFS.Types as IPFS
import           Fission.IPFS.CID.Types
import           Fission.User        as User

import           Fission.Web.Server
import           Fission.Web.Error as Web.Err

type API = Capture "cid" CID
        :> PutAccepted '[PlainText, OctetStream] AWS.DomainName

server ::
  ( HasLogFunc         cfg
  , Has IPFS.Gateway   cfg
  , Has AWS.AccessKey  cfg
  , Has AWS.SecretKey  cfg
  , Has AWS.ZoneID     cfg
  , Has AWS.DomainName cfg
  )
  => User
  -> RIOServer cfg API
server User { username } cid = 
  registerDomain username cid
    >>= Web.Err.ensureM
