{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Fission.Web.Client.App
  ( index
  , create
  , update
  , destroy
  ) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.App.Types

index :<|> create :<|> update :<|> destroy = client $ Proxy @App
