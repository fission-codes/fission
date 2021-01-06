module Fission.Web.Server.Handler.Heroku (handler) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import qualified Fission.Web.Server.Heroku.AddOn               as Heroku.AddOn
import qualified Fission.Web.Server.LoosePin                   as LoosePin
import           Fission.Web.Server.Reflective
import qualified Fission.Web.Server.User                       as User

import qualified Fission.Web.Server.Handler.Heroku.Deprovision as Deprovision
import qualified Fission.Web.Server.Handler.Heroku.Provision   as Provision

handler ::
  ( MonadReflectiveServer    m
  , MonadRemoteIPFS          m
  , MonadLocalIPFS           m
  , MonadLogger              m
  , MonadThrow               m
  , User.Creator             m
  , MonadDB                t m
  , MonadLogger            t
  , MonadThrow             t
  , User.Retriever         t
  , User.Destroyer         t
  , LoosePin.Retriever     t
  , LoosePin.Destroyer     t
  , Heroku.AddOn.Destroyer t
  , Heroku.AddOn.Retriever t
  )
  => ServerT API m
hanlder = Provision.create :<|> Deprovision.destroy
