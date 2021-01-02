module Fission.Web.Handler
  ( fromHandler
  , toHandler
  ) where

import           Control.Monad.Except
import           Servant

import           Fission.Prelude

-- | Natural transformation to native Servant handler
toHandler :: (m a -> IO a) -> m a -> Handler a
toHandler runner actions = Handler . ExceptT . try $ runner actions

-- | Natural transformation into a RIO handler
fromHandler :: MonadIO m => Handler a -> m a
fromHandler handler =
  liftIO <| runHandler handler >>= \case
    Right inner     -> pure inner
    Left servantErr -> throwM servantErr
