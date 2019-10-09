module Fission.Web.Server
  ( RIOServer
  , fromHandler
  , toHandler
  ) where

import RIO hiding (Handler)
import Servant

type RIOServer cfg api = ServerT api (RIO cfg)

-- | Natural transformation to native Servant handler
toHandler :: cfg -> RIO cfg a -> Handler a
toHandler cfg = liftIO . runRIO cfg

-- | Natural transformation into a RIO handler
fromHandler :: Handler a -> RIO cfg a
fromHandler handler =
  liftIO $ runHandler handler >>= \case
    Right inner     -> pure inner
    Left servantErr -> throwM servantErr
