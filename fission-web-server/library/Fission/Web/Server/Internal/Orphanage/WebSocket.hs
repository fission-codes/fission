{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.Server.Internal.Orphanage.WebSocket () where

import           Data.Swagger

import           Servant.API.WebSocket
import           Servant.Ekg
import           Servant.Swagger

import           Fission.Prelude

instance HasSwagger WebSocket where
  toSwagger _ =  mempty |> schemes ?~ [Wss]

instance HasEndpoint WebSocket where
  getEndpoint      _ _ = Just $ APIEndpoint [] "WebSocket"
  enumerateEndpoints _ =       [APIEndpoint [] "WebSocket"]
