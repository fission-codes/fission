{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Fission.Web.Client.IPFS
  ( cid
  , dag
  , getPeers
  , upload
  , pin
  , download
  ) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.IPFS.Types

cid :<|> dag :<|> getPeers :<|> upload :<|> pin :<|> download = client $ Proxy @IPFS
