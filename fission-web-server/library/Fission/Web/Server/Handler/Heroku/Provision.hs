module Fission.Web.Server.Handler.Heroku.Provision (create) where

import qualified Data.Text                                       as Text
import           Data.UUID                                       as UUID
import qualified RIO.NonEmpty                                    as NonEmpty

import           Database.Persist.Sql

import           Network.IPFS
import           Servant

import           Fission.Prelude

import           Fission.Platform.Heroku.Provision.Request.Types
import           Fission.Platform.Heroku.Provision.Types

import qualified Fission.Random                                  as Random
import           Fission.Security.Types                          (Secret (..))
import           Fission.User.Password.Types
import qualified Fission.User.Provision.Types                    as User
import           Fission.User.Username.Types

import qualified Fission.Web.API.Heroku.Provision.Types          as API.Heroku
import qualified Fission.Web.API.Host.Types                      as Web

import qualified Fission.Web.Server.Error                        as Web.Err
import           Fission.Web.Server.IPFS.Linked
import           Fission.Web.Server.Reflective
import qualified Fission.Web.Server.User.Creator                 as User

create ::
  ( MonadIO               m
  , MonadThrow            m
  , MonadLogger           m
  , MonadRemoteIPFS       m
  , MonadLinkedIPFS       m
  , MonadReflectiveServer m
  , User.Creator          m
  )
  => ServerT API.Heroku.Provision m
create Request {uuid, region} _ = do
  username      <- Web.Err.ensure . mkUsername . Text.pack $ UUID.toString uuid
  now           <- getCurrentTime
  secret        <- Random.alphaNum 50
  userID        <- Web.Err.ensureM $ User.createWithHeroku uuid region username (Password secret) now
  Web.Host url' <- getHost
  ipfsPeers     <- getLinkedPeers

  return Provision
    { id      = fromIntegral $ fromSqlKey userID
    , peers   = NonEmpty.toList ipfsPeers
    , message = "Successfully provisioned Interplanetary Fission!"
    , config  = User.Provision
      { username = username
      , password = Secret secret
      , url      = url'
      }
    }
