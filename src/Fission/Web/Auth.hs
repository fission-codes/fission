{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Auth
  ( server
  , context
  , basic
  , user
  , check
  ) where

import RIO

import Servant
import Database.Selda

import Fission.Internal.UTF8
import Fission.Security
import Fission.Web.Server
import Fission.User as User
import Fission.Storage.SQLite

server :: HasServer api '[BasicAuthCheck ByteString]
       => Proxy api
       -> cfg
       -> RIOServer cfg api
       -> Server api
server api cfg = hoistServerWithContext api context (toHandler cfg)

context :: Proxy (BasicAuthCheck ByteString ': '[])
context = Proxy

basic :: ByteString -> ByteString -> Context (BasicAuthCheck ByteString ': '[])
basic unOK pwOK = BasicAuthCheck check' :. EmptyContext
  where
    check' (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then return $ Authorized username
         else return Unauthorized

user :: MonadSelda IO => Context (BasicAuthCheck User ': '[])
user = BasicAuthCheck check :. EmptyContext

check :: MonadSelda m => BasicAuthData -> m (BasicAuthResult User)
check (BasicAuthData username password) = do
  mayUsr <- getOne . User.bySecret $ decodeUtf8Lenient password
  return $ maybe NoSuchUser checkId mayUsr

  where
    checkId usr@(User { _id })
      | encodeUtf8 (digest _id) == username = Authorized usr
      | otherwise                          = Unauthorized
