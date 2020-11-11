{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.WebSocket () where

import           Data.Swagger

import           Servant.API.WebSocket
import           Servant.Swagger

import           Fission.Prelude

instance HasSwagger WebSocket where
  toSwagger _ =  mempty |> schemes ?~ [Wss]
