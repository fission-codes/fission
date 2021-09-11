module Fission.Web.Server.Handler.User.DataRoot (handlerV2, handlerV_) where

import           Network.IPFS.CID.Types

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.User.DataRoot.Types    as DataRoot

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error               as Web.Error
import qualified Fission.Web.Server.User                as User
import           Fission.Web.Server.WNFS                as WNFS

handlerV2 ::
  ( MonadLogger   m
  , MonadThrow    m
  , MonadTime     m
  , MonadWNFS     m
  , User.Modifier m
  )
  => DataRoot.RoutesV2 (AsServerT m)
handlerV2 = DataRoot.RoutesV2 {get, update}

handlerV_ ::
  ( MonadLogger   m
  , MonadThrow    m
  , MonadTime     m
  , MonadWNFS     m
  , User.Modifier m
  )
  => DataRoot.RoutesV_ (AsServerT m)
handlerV_ = DataRoot.RoutesV_ {get, update}

get :: (MonadLogger m, MonadThrow m, MonadWNFS m) => User.Username -> m CID
get username = Web.Error.ensureM $ WNFS.getUserDataRoot username

update ::
  ( MonadTime     m
  , MonadLogger   m
  , MonadThrow    m
  , User.Modifier m
  )
  => CID
  -> Authorization
  -> m NoContent
update newCID Authorization {about = Entity userID _} = do
  now <- currentTime
  Web.Error.ensureM $ User.setData userID newCID now
  return NoContent
