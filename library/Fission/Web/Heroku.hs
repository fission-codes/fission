module Fission.Web.Heroku
  ( API
  , server
  ) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import qualified Fission.Web.Heroku.Provision   as Provision
import qualified Fission.Web.Heroku.Deprovision as Deprovision
import           Fission.Web.Server.Reflective
import qualified Fission.User     as User
import qualified Fission.User.CID as User.CID

type API = Provision.API :<|> Deprovision.API

server ::
  ( User.CID.MonadDBMutation m
  -- , User.CID.Queryable    m
  , User.MonadDBMutation     m
  , User.MonadDBQuery        m
  , MonadThrow               m
  , MonadLogger              m
  , MonadLocalIPFS           m
  , MonadRemoteIPFS          m
  , MonadReflectiveServer    m
  )
  => ServerT API m
server = Provision.create
    :<|> Deprovision.destroy
