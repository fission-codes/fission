{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Fission.Web.Client.App
  ( index
  , create
  , update
  , streamingUpdate
  , destroy
  ) where

import qualified Servant.Client.Streaming  as Streaming

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.App.Types as App

index :<|> create :<|> update :<|> destroy = client $ Proxy @App.NonStreaming
streamingUpdate =  Streaming.client $ Proxy @App.Streaming
