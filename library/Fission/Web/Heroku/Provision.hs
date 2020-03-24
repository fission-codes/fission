module Fission.Web.Heroku.Provision
  ( API
  , create
  ) where

import           Data.UUID as UUID
import qualified Data.Text as Text

import           Network.IPFS
import           Network.IPFS.Peer (getExternalAddress)
import           Network.IPFS.Peer.Types as IPFS

import           Servant

import           Fission.Prelude

import qualified Fission.Web.Error                          as Web.Err
import qualified Fission.Web.Heroku.MIME.VendorJSONv3.Types as Heroku
import qualified Fission.Web.Types                          as Web
import           Fission.Web.Server.Reflective

import           Fission.Platform.Heroku.Provision.Types
import           Fission.Platform.Heroku.Provision.Request.Types

import qualified Fission.Random               as Random
import           Fission.Security.Types       (Secret (..))
import qualified Fission.User.Provision.Types as User
import qualified Fission.User.Creator         as User
import           Fission.User.Username.Types
import           Fission.User.Password.Types

type API
  =  Summary "Provision"
  :> Description "Provision a new Heroku add-on (for the Heroku partner service only)"
  :> ReqBody '[JSON]                Request
  :> Post    '[Heroku.VendorJSONv3] Provision

create ::
  ( MonadTime               m
  , MonadThrow              m
  , MonadLogger             m
  , MonadLocalIPFS          m
  , MonadReflectiveServer   m
  , MonadDB               t m
  , User.Creator          t
  )
  => ServerT API m
create Request {uuid, region} = do
  let username = Username <| Text.pack <| UUID.toString uuid
  secret <- Random.alphaNum 50

  secret
    |> Password
    |> User.createWithHeroku uuid region username
    |> runDBNow
    |> bind Web.Err.ensure
    |> bind \userID -> do
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
