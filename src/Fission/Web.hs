{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web where

import RIO

import Servant as Servant

import Data.Has

import Fission.Config
import Fission.Web.Server

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping

type API = "ping" :> Ping.API
      :<|> "ipfs" :> Servant.BasicAuth "admin realm" Text {- TODO `User` -} :> IPFS.API

app :: (Has IpfsPath cfg, HasLogFunc cfg) => cfg -> Application
app cfg = serveWithContext api basicAuthContext $ hoistedServer cfg

hoistedServer :: (Has IpfsPath cfg, HasLogFunc cfg) => cfg -> Server API
hoistedServer cfg = hoistServerWithContext api pxyCtx (toHandler cfg) server

pxyCtx :: Proxy (BasicAuthCheck Text ': '[])
pxyCtx = Proxy

basicAuthContext :: Context (BasicAuthCheck Text ': '[])
basicAuthContext = checkText :. EmptyContext

checkText :: BasicAuthCheck Text
checkText =
  let
    check (BasicAuthData _username _password) = return $ Unauthorized -- Authorized ("yup" :: Text)
  in
    BasicAuthCheck check

checkBasicAuth :: Text -> BasicAuthResult ()
checkBasicAuth _ = Authorized ()

server :: (Has IpfsPath cfg, HasLogFunc cfg) => RIOServer cfg API
server = Ping.server :<|> guarded IPFS.server

api :: Proxy API
api = Proxy

guarded :: Has IpfsPath cfg => RIOServer cfg IPFS.API -> Text -> RIOServer cfg IPFS.API
guarded s "password" = s
guarded _ _          = error "boom" -- throwM err401
