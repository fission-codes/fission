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
  lazyBody <- strictRequestBody req

  let
    oldVault      = vault req
    body          = Lazy.toStrict lazyBody
    vaultWithHash = addBodyHash body vaultKey oldVault

  ioBody <- newIORef body

  let
    newReq = req
      { requestBody = atomicModifyIORef ioBody \x -> (mempty, x)
      , vault       = vaultWithHash
      }

  app newReq sendResponse

addBodyHash :: ByteString -> Vault.Key ByteString -> Vault.Vault -> Vault.Vault
addBodyHash body vaultKey oldVault = 
    if BS.null body
      then oldVault
      else Vault.insert vaultKey hash oldVault
  where 
    hash = 
      body 
        |> digest
        |> encodeUtf8
