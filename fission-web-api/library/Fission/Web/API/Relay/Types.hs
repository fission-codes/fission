 module Fission.Web.API.Relay.Types (Routes (..)) where

import           Servant.API.Generic
import           Servant.API.WebSocket

import           Web.DID.Types

import           Fission.Web.API.Prelude

newtype Routes mode = Routes { socket :: mode :- Capture "did" DID :> WebSocket }
  deriving Generic

