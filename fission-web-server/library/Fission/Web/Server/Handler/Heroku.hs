module Fission.Web.Server.Handler.Heroku (handler) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import qualified Fission.LoosePin                      as LoosePin
import qualified Fission.Platform.Heroku.AddOn         as Heroku.AddOn
import qualified Fission.User                          as User

import qualified Fission.Web.Server.Heroku.Deprovision as Deprovision
import qualified Fission.Web.Server.Heroku.Provision   as Provision
import           Fission.Web.Server.Reflective

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
