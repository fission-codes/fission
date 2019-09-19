-- | Web error handling, common patterns, and other helpers
module Fission.Web.Error
  ( ToServerError (..)
  , ensure
  , ensure_
  , throw
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Network.HTTP.Types.Status
import Servant.Server

import Fission.Internal.Constraint

class ToServerError err where
  toServerError :: err -> ServerError

ensure :: MonadRIO   cfg m
       => HasLogFunc cfg
       => MonadThrow     m
       => Display       err
       => ToServerError err
       => Either err a
       -> m a
ensure = either throw pure

ensureM :: MonadRIO   cfg m
        => MonadThrow     m
        => ServerError
        -> Maybe a
        -> m a
ensureM err = maybe (throwM err) pure

throw :: MonadRIO   cfg m
      => HasLogFunc cfg
      => MonadThrow     m
      => Display       err
      => ToServerError err
      => err
      -> m a
throw err = do
  let
    serverError@(ServerError {..}) = toServerError err
    status      = Status errHTTPCode (Lazy.toStrict errBody)

  when (statusIsServerError status) (logError $ display err)
  throwM serverError
