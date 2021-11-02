module Network.IPFS.Peer.Error (Error (..)) where

import           Network.IPFS.Peer.Types
import           Network.IPFS.Prelude

data Error
  = DecodeFailure    String
  | CannotConnect    Peer
  | CannotDisconnect Peer
  | UnknownErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           )

instance Display Error where
  display = \case
    DecodeFailure    err  -> "Unable to decode: " <> displayShow err
    CannotConnect    peer -> "Unable to connect to " <> display peer
    CannotDisconnect peer -> "Unable to disconnect from " <> display peer
    UnknownErr       msg  -> "Unknown IPFS peer list error: " <> display msg
