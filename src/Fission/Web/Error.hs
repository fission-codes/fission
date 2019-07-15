-- | Web error handling, common patterns, and other helpers
module Fission.Web.Error
  ( ensure
  , throw
  ) where

import RIO

import Data.Aeson
import Network.HTTP.Types.Status
import Servant.Exception

import Fission.Internal.Constraint

ensure :: MonadRIO   cfg m
       => HasLogFunc cfg
       => MonadThrow     m
       => Display      err
       => Exception    err
       => ToJSON       err
       => ToServantErr err
       => Either err b
       -> m ()
ensure = either throw (pure . const ())

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
