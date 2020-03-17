-- | Web error handling, common patterns, and other helpers
module Fission.Web.Error
  ( ensure
  , ensureM
  , ensureMaybe
  , throw
  , module Fission.Web.Error.Class
  ) where

import           Network.HTTP.Types.Status
import qualified RIO.ByteString.Lazy as Lazy
import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

import           Fission.Internal.Orphanage.ServerError ()

ensure ::
  ( MonadLogger m
  , MonadThrow  m
  , Display       err
  , ToServerError err
  )
  => Either err a
  -> m a
ensure = either throw pure

ensureM :: (MonadThrow m, Exception err) => Either err a -> m a
ensureM = either throwM pure

ensureMaybe :: MonadThrow m => ServerError -> Maybe a -> m a
ensureMaybe err = maybe (throwM err) pure

throw ::
  ( MonadLogger m
  , MonadThrow  m
  , Display       err
  , ToServerError err
  )
  => err
  -> m a
throw err = do
  let
    serverError@(ServerError {..}) = toServerError err
    status = Status errHTTPCode <| Lazy.toStrict errBody

  when (statusIsServerError status) do
    logError <| textDisplay err

  throwM serverError
