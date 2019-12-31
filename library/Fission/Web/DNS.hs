module Fission.Web.DNS
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Network.AWS
import qualified Network.IPFS.Types as IPFS
import           Network.IPFS.CID.Types

import           Fission.Models
import           Fission.Prelude

import           Fission.AWS.Route53
import qualified Fission.AWS.Types   as AWS

import           Fission.Web.Error as Web.Err

type API = Capture "cid" CID
        :> PutAccepted '[PlainText, OctetStream] AWS.DomainName

server ::
  ( Has IPFS.Gateway           cfg
  -- , Has AWS.AccessKey          cfg
  -- , Has AWS.SecretKey          cfg
  , Has AWS.ZoneID             cfg
  , Has AWS.DomainName         cfg
  , Has AWS.Route53MockEnabled cfg
  , MonadReader                cfg m
  , MonadAWS      m
  , MonadLogger   m
  , MonadTime     m
  , MonadUnliftIO m
  -- , MonadThrow    m
  )
  => Entity User
  -> ServerT API m
server (Entity _id User { userUsername }) cid =
  Web.Err.ensureM =<< registerDomain userUsername cid
