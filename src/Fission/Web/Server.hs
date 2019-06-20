module Fission.Web.Server
  ( RIOServer
  , fromHandler
  , toHandler
  ) where

import RIO hiding (Handler)
import Servant -- (Handler, ServerT, runHandler, hoistServer, HasServer)

type RIOServer cfg api = ServerT api (RIO cfg)

-- | Natural transformation `RIO cfg -> Handler`
toHandler :: cfg -> RIO cfg a -> Handler a
toHandler cfg = liftIO . runRIO cfg

-- | Natural transformation `Handler -> RIO cfg`
-- fromServer :: forall api cfg. HasServer api '[] => Handler api -> RIOServer cfg api
-- fromServer :: HasServer api '[] => Proxy api -> Server api -> RIOServer cfg api
-- fromServer server = hoistServer (Proxy :: Proxy api) fromHandler server

fromHandler :: Handler a -> RIO cfg a
fromHandler handler =
  liftIO $ runHandler handler >>= \case
    Right inner     -> pure inner
    Left servantErr -> throwM servantErr
