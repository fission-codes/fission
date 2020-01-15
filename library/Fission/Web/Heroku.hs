module Fission.Web.Heroku
  ( API
  , server
  ) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import qualified Fission.User                  as User
import qualified Fission.User.CID              as User.CID
import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn

import qualified Fission.Web.Heroku.Provision   as Provision
import qualified Fission.Web.Heroku.Deprovision as Deprovision
import           Fission.Web.Server.Reflective

type API = Provision.API :<|> Deprovision.API

server ::
  ( MonadReflectiveServer    m
  , MonadRemoteIPFS          m
  , MonadLocalIPFS           m
  , MonadLogger              m
  , MonadThrow               m
  , MonadTime                m
  , MonadDB                t m
  , MonadLogger            t
  , MonadThrow             t
  , User.Creator           t
  , User.Retriever         t
  , User.Destroyer         t
  , User.CID.Retriever     t
  , User.CID.Destroyer     t
  , Heroku.AddOn.Destroyer t
  , Heroku.AddOn.Retriever t
  )
  => ServerT API m
server = Provision.create
    :<|> Deprovision.destroy
