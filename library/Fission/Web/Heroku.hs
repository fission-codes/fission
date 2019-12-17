module Fission.Web.Heroku
  ( API
  , server
  ) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import qualified Fission.Web.Types              as Web
import qualified Fission.Web.Heroku.Provision   as Provision
import qualified Fission.Web.Heroku.Deprovision as Deprovision

type API = Provision.API :<|> Deprovision.API

server ::
  ( MonadDB          m
  , MonadLocalIPFS   m
  , MonadRemoteIPFS  m
  , MonadThrow       m
  , MonadLogger      m
  , MonadTime        m
  , MonadReader  cfg m
  , Has Web.Host cfg
  )
  => ServerT API m
server = Provision.create
    :<|> Deprovision.destroy
