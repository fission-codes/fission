module Fission.Web.Server.Handler.Heroku.Provision (create) where

import qualified Data.Text                                         as Text
import           Data.UUID                                         as UUID

import           Network.IPFS
import           Network.IPFS.Peer                                 (getExternalAddress)
import           Network.IPFS.Peer.Types                           as IPFS

import           Servant

import           Fission.Prelude

import qualified Fission.Web.Server.Error                          as Web.Err
import qualified Fission.Web.Server.Heroku.MIME.VendorJSONv3.Types as Heroku
import           Fission.Web.Server.Reflective
import qualified Fission.Web.Server.Types                          as Web

import           Fission.Heroku.Provision.Request.Types
import           Fission.Heroku.Provision.Types

import qualified Fission.Random                                    as Random
import           Fission.Security.Types                            (Secret (..))
import qualified Fission.User.Creator                              as User
import           Fission.User.Password.Types
import qualified Fission.User.Provision.Types                      as User
import           Fission.User.Username.Types

create ::
  ( MonadIO               m
  , MonadThrow            m
  , MonadLogger           m
  , MonadLocalIPFS        m
  , MonadReflectiveServer m
  , User.Creator          m
  )
  => ServerT API m
create Request {uuid, region} = do
  username      <- Web.Err.ensure . mkUsername . Text.pack $ UUID.toString uuid
  now           <- getCurrentTime
  secret        <- Random.alphaNum 50
  userID        <- Web.Err.ensureM $ User.createWithHeroku uuid region username (Password secret) now
  Web.Host url' <- getHost
  ipfsPeers     <- getIPFSPeers

  return Provision
    { id      = userID
    , peers   = ipfsPeers
    , message = "Successfully provisioned Interplanetary Fission!"
    , config  = User.Provision
      { username = username
      , password = Secret secret
      , url      = url'
      }
    }

getIPFSPeers :: (MonadLocalIPFS m, MonadLogger m) => m [IPFS.Peer]
getIPFSPeers =
  getExternalAddress >>= \case
    Right peers' ->
      pure peers'

    Left err -> do
      logError <| textShow err
      return []
