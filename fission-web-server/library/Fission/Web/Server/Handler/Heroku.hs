module Fission.Web.Server.Handler.Heroku (handler) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.Heroku.Types                  as API

import qualified Fission.Web.Server.Heroku.AddOn               as Heroku.AddOn
import           Fission.Web.Server.IPFS.Linked
import qualified Fission.Web.Server.LoosePin                   as LoosePin
import           Fission.Web.Server.Reflective
import qualified Fission.Web.Server.User                       as User

import qualified Fission.Web.Server.Handler.Heroku.Deprovision as Deprovision
import qualified Fission.Web.Server.Handler.Heroku.Provision   as Provision

import           Fission.Web.Server.MonadDB

handler ::
  ( MonadReflectiveServer    m
  , MonadRemoteIPFS          m
  , MonadLinkedIPFS          m
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
  => ServerT API.Heroku m
handler = Provision.create :<|> Deprovision.destroy
