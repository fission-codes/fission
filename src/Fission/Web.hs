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
import Servant.Auth.Server as Auth

import Data.Has

import Fission.Config
import Fission.Web.Server

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping

type API = "ping" :> Ping.API
      :<|> "ipfs" :> Servant.BasicAuth "admin realm" Text {- TODO `User` -} :> IPFS.API

app :: (HasContextEntry '[] (BasicAuthCheck Text), Has IpfsPath cfg, HasLogFunc cfg) => cfg -> Application
app cfg = serveWithContext api ctx $ hoistServerWithAuth api (toHandler cfg) server
  where ctx = basicAuthServerContext -- checkBasicAuth :. EmptyContext

basicAuthServerContext :: Context (BasicAuthCheck Text ': '[])
basicAuthServerContext = checkText :. EmptyContext

hoistServerWithAuth
  :: HasServer api '[Auth.BasicAuth]
  => Proxy api
  -> (forall x. m x -> n x) -- natural transformation
  -> ServerT api m
  -> ServerT api n
hoistServerWithAuth api' =
  hoistServerWithContext api' (Proxy :: Proxy '[Auth.BasicAuth])

checkText :: BasicAuthCheck Text
checkText =
  let
    check (BasicAuthData _username _password) = return $ Authorized ("yup" :: Text)
  in
    BasicAuthCheck check

checkBasicAuth :: Text -> BasicAuthResult ()
checkBasicAuth _ = Authorized ()

server :: (Has IpfsPath cfg, HasLogFunc cfg) => RIOServer cfg API
server = Ping.server :<|> (guarded IPFS.server)

api :: Proxy API
api = Proxy

guarded :: Has IpfsPath cfg => RIOServer cfg IPFS.API -> Text -> RIOServer cfg IPFS.API
guarded s "password" = s
guarded _ _          = error "boom" -- throwM err401
