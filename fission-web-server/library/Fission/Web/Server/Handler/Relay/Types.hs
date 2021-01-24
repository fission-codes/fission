module Fission.Web.Server.Handler.Relay.Types (RelayWS) where

import           Servant
import           Servant.API.WebSocket

import           Fission.User.DID.Types

type RelayWS = Capture "did" DID :> WebSocket
