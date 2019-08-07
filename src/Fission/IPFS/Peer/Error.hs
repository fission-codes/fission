module Fission.IPFS.Peer.Error (Error (..)) where

import RIO

import Network.HTTP.Types.Status
import Servant.Exception

import Fission.Internal.Orphanage ()

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

instance ToServantErr Error where
  status = \case
    DecodeFailure _ -> internalServerError500
    UnknownErr    _ -> internalServerError500

  message = \case
    DecodeFailure _ -> "Peer list decode error"
    UnknownErr    _ -> "Unknown peer list error"
