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

type API = Provision.API :<|> Deprovision.API

server ::
  ( MonadDBQuery User        m
  , MonadDBQuery UserCid     m
  , MonadDBQuery HerokuAddOn m
  , MonadTime                m
  , MonadThrow               m
  , MonadLogger              m
  , MonadLocalIPFS           m
  , MonadRemoteIPFS          m
  , MonadReflectiveServer    m
  )
  => ServerT API m
server = Provision.create
    :<|> Deprovision.destroy
