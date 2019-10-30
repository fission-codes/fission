-- | Web error handling, common patterns, and other helpers
module Fission.Web.Error
  ( ToServerError (..)
  , ensure
  , ensureM
  , ensureMaybe
  , throw
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Network.HTTP.Types.Status
import Servant.Server

import Fission.Internal.Constraint

class ToServerError err where
  toServerError :: err -> ServerError

instance ToServerError ServerError where
  toServerError = id

instance ToServerError Int where
  toServerError = \case
    300 -> err300
    301 -> err301
    302 -> err302
    303 -> err303
    304 -> err304
    305 -> err305
    307 -> err307

    400 -> err400
    401 -> err401
    402 -> err402
    403 -> err403
    404 -> err404
    405 -> err405
    406 -> err406
    407 -> err407
    409 -> err409
    410 -> err410
    411 -> err411
    412 -> err412
    413 -> err413
    414 -> err414
    415 -> err415
    416 -> err416
    417 -> err417
    418 -> err418
    422 -> err422

    500 -> err500
    501 -> err501
    502 -> err502
    503 -> err503
    504 -> err504
    505 -> err505

    n -> error $ show n <> " is not an error status code"

ensure :: MonadRIO   cfg m
       => HasLogFunc cfg
       => MonadThrow     m
       => Display       err
       => Exception     err
       => ToServerError err
       => Either err a
       -> m a
ensure = either throw pure

ensureM :: MonadRIO   cfg m
        => MonadThrow     m
        => Exception err
        => Either err a
        -> m a
ensureM = either throwM pure

ensureMaybe :: MonadRIO   cfg m
            => MonadThrow     m
            -- => Exception err
            => ServerError
            -> Maybe a
            -> m a
ensureMaybe err = maybe (throwM err) pure

throw :: MonadRIO   cfg m
      => HasLogFunc cfg
      => MonadThrow     m
      => Exception     err
      => Display       err
      => ToServerError err
      => err
      -> m a
throw err = do
  -- let
  --   serverError@(ServerError {..}) = toServerError err
  --   status = Status errHTTPCode $ Lazy.toStrict errBody

  -- when (statusIsServerError status) (logError $ display err)
  throwM err
