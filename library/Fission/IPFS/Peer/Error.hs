module Fission.IPFS.Peer.Error (Error (..)) where

import RIO

import Servant.Server

import Fission.Web.Error
import Fission.IPFS.Peer.Types

data Error
  = DecodeFailure String
  | CannotConnect Peer
  | UnknownErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           )

instance Display Error where
  display = \case
    DecodeFailure err  -> "Unable to decode: " <> displayShow err
    CannotConnect peer -> "Unable to connect to " <> display peer
    UnknownErr    msg  -> "Unknown IPFS peer list error: " <> display msg

instance ToServerError Error where
  toServerError = \case
    DecodeFailure _ -> err500 { errBody = "Peer list decode error" }
    CannotConnect _ -> err500 { errBody = "Unable to connect to peer" }
    UnknownErr    _ -> err500 { errBody = "Unknown peer list error" }
