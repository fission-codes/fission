module Fission.IPFS.Peer.Error (Error (..)) where

import RIO

import Servant.Server

-- import Fission.Internal.Orphanage ()
import Fission.Web.Error

data Error
  = DecodeFailure String
  | UnknownErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           )

instance Display Error where
  display = \case
    DecodeFailure err -> "Unable to decode: " <> displayShow err
    UnknownErr    txt -> "Unknown IPFS peer list error: " <> display txt

instance ToServerError Error where
  toServerError = \case
    DecodeFailure _ -> err500 { errBody = "Peer list decode error" }
    UnknownErr    _ -> err500 { errBody = "Unknown peer list error" }
