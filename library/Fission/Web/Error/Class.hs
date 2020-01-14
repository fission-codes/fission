module Fission.Web.Error.Class (ToServerError (..)) where

import           Servant.Server

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8

import           Network.IPFS.Types
import           Network.IPFS.Error

import qualified Network.IPFS.Add.Error  as Add
import qualified Network.IPFS.Get.Error  as Get
import qualified Network.IPFS.Peer.Error as Peer

class ToServerError err where
  toServerError :: err -> ServerError

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

    n -> error (show n <> " is not an error status code")

instance ToServerError Error where
  toServerError = \case
    AddErr           addErr -> toServerError addErr
    GetErr           getErr -> toServerError getErr
    LinearizationErr linErr -> toServerError linErr

instance ToServerError Get.Error where
  toServerError = \case
    Get.InvalidCID       txt          -> err422 { errBody = UTF8.textToLazyBS txt }
    Get.UnexpectedOutput _            -> err502 { errBody = "Unexpected IPFS result" }
    Get.UnknownErr       _            -> err502 { errBody = "Unknown IPFS error" }
    Get.TimedOut         (CID hash) _ -> err504 { errBody = "IPFS timed out looking for " <> UTF8.textToLazyBS hash }

instance ToServerError Add.Error where
  toServerError = \case
    Add.InvalidFile        -> err422 { errBody = "File not processable by IPFS" }
    Add.UnknownAddErr    _ -> err502 { errBody = "Unknown IPFS error" }
    Add.RecursiveAddErr  _ -> err502 { errBody = "Error while adding directory" }
    Add.UnexpectedOutput _ -> err502 { errBody = "Unexpected IPFS result" }
    Add.IPFSDaemonErr  msg -> err502 { errBody = "IPFS Daemon Error: " <> UTF8.textToLazyBS msg}

instance ToServerError Peer.Error where
  toServerError = \case
    Peer.DecodeFailure _ -> err500 { errBody = "Peer list decode error" }
    Peer.CannotConnect _ -> err503 { errBody = "Unable to connect to peer" }
    Peer.UnknownErr    _ -> err500 { errBody = "Unknown peer list error" }

instance ToServerError Linearization where
  toServerError _ = err500 { errBody = "Unable to linearize IPFS result" }

instance ToServerError a => ToServerError (OpenUnion '[a]) where
  toServerError = catchesOpenUnion toServerError

instance (ToServerError a, ToServerError b) => ToServerError (OpenUnion '[a, b]) where
  toServerError = catchesOpenUnion (toServerError, toServerError)

instance
  ( ToServerError a
  , ToServerError b
  , ToServerError c
  )
  => ToServerError (OpenUnion '[a, b, c]) where
  toServerError = catchesOpenUnion (toServerError, toServerError, toServerError)

instance
  ( ToServerError a
  , ToServerError b
  , ToServerError c
  , ToServerError d
  )
  => ToServerError (OpenUnion '[a, b, c, d]) where
  toServerError = catchesOpenUnion (toServerError, toServerError, toServerError, toServerError)

-- instance
--   ( ToServerError a
--   , ToServerError b
--   , ToServerError c
--   , ToServerError d
--   , ToServerError e
--   )
--   => ToServerError (OpenUnion '[a, b, c, d, e]) where
--   toServerError = catchesOpenUnion
--     ( toServerError
--     , toServerError
--     , toServerError
--     , toServerError
--     , toServerError
--     )
