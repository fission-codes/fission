module Fission.Web.Server
  ( RIOServer
  , fromHandler
  , toHandler
  ) where

import Control.Monad.Except
import Servant

import Fission.Prelude

type RIOServer cfg api = ServerT api (RIO cfg)

-- | Natural transformation to native Servant handler
toHandler :: cfg -> RIO cfg a -> Handler a
toHandler cfg a = Handler . ExceptT . try <| runReaderT (unRIO a) cfg

-- | Natural transformation into a RIO handler
fromHandler :: Handler a -> RIO cfg a
fromHandler handler =
  liftIO <| runHandler handler >>= \case
    Right inner     -> pure inner
    Left servantErr -> throwM servantErr
