-- | Web error handling, common patterns, and other helpers
module Fission.Web.Error
  ( ensure
  , ensure_
  , throw
  ) where

import RIO

import Data.Aeson
import Network.HTTP.Types.Status
import Servant.Exception
import Servant.Server

import Fission.Internal.Constraint

ensure :: MonadRIO   cfg m
       => HasLogFunc cfg
       => MonadThrow     m
       => Display      err
       => Exception    err
       => ToJSON       err
       => ToServantErr err
       => Either err a
       -> m a
ensure = either throw pure

ensure_ :: MonadThrow m => Maybe a -> m a
ensure_ = \case
  Nothing -> throwM err404
  Just a  -> pure a

throw :: MonadRIO   cfg m
      => HasLogFunc cfg
      => MonadThrow     m
      => Display      err
      => Exception    err
      => ToJSON       err
      => ToServantErr err
      => err
      -> m a
throw err = do
  when (statusIsServerError $ status err) (logError $ display err)
  throwM $ toServantException err
