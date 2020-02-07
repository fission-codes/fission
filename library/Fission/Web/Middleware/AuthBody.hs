{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Fission.Web.Middleware.AuthBody (middleware) where

import Fission.Prelude

import Fission.Security

import Network.Wai (Middleware, strictRequestBody)
import Network.Wai.Internal

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as Lazy

import qualified Data.Vault.Lazy as Vault

middleware :: Vault.Key ByteString -> Middleware
middleware vaultKey app req sendResponse = do
  body   <- Lazy.toStrict <$> strictRequestBody req
  ioBody <- newIORef body

  let
    vaultWithHash = addBodyHash body vaultKey oldVault
    oldVault      = vault req
    newReq        = req
      { requestBody = atomicModifyIORef ioBody \x -> (mempty, x)
      , vault       = vaultWithHash
      }

  app newReq sendResponse

addBodyHash :: ByteString -> Vault.Key ByteString -> Vault.Vault -> Vault.Vault
addBodyHash body vaultKey oldVault =
  if BS.null body
    then oldVault
    else Vault.insert vaultKey (encodeUtf8 <| digest body) oldVault
