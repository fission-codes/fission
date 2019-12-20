{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Fission.Web.Middleware.AuthBody (foo) where

import Fission.Prelude

import Network.Wai (Middleware, strictRequestBody)
import Network.Wai.Internal

import qualified Data.ByteString.Lazy       as Lazy

import qualified Data.Vault.Lazy as Vault

-- type Middleware = Application -> Application
-- type Application =
--    Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

-- how to generate a unique vault key outside this function:
-- `vaultKey <- newKey`

foo :: Vault.Key Text -> Middleware
foo vaultKey app req sendResponse = do
  lazyBody <- strictRequestBody req

  let
    oldVault      = vault req
    body          = Lazy.toStrict lazyBody
    hash          = undefined -- TODO hash the body
    vaultWithHash = Vault.insert vaultKey hash oldVault

  ioBody <- newIORef body

  let
    newReq = req
      { requestBody = atomicModifyIORef ioBody \x -> (mempty, x)
      , vault       = vaultWithHash
      }

  app newReq sendResponse
