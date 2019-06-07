{-# LANGUAGE MonoLocalBinds #-}
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
  , checkUser
  , nt
  ) where

import RIO hiding (id)

import Servant
import Database.Selda

import Fission.Internal.Orphanage ()
import Fission.Security
import Fission.Storage.SQLite
import Fission.User as User
import Fission.Web.Server

server :: HasServer api '[BasicAuthCheck User]
       => Proxy api
       -> cfg
       -> RIOServer cfg api
       -> Server api
server api cfg = hoistServerWithContext api context (toHandler cfg)

context :: Proxy (BasicAuthCheck User ': '[])
context = Proxy

basic :: ByteString -> ByteString -> Context (BasicAuthCheck ByteString ': '[])
basic unOK pwOK = BasicAuthCheck check' :. EmptyContext
  where
    check' (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then return $ Authorized username
         else return Unauthorized

user :: MonadSelda (RIO cfg) => RIO cfg (Context (BasicAuthCheck User ': '[]))
user = ask >>= \cfg -> pure (BasicAuthCheck (nt checkUser cfg) :. EmptyContext)

nt :: MonadIO m
   => (BasicAuthData -> RIO cfg (BasicAuthResult u))
   -> cfg
   -> BasicAuthData
   -> m (BasicAuthResult u)
nt check cfg = runRIO cfg . check

checkUser :: MonadSelda (RIO cfg) => BasicAuthData -> RIO cfg (BasicAuthResult User)
checkUser (BasicAuthData username password) = do
  mayUsr <- getOne . User.bySecret $ decodeUtf8Lenient password
  return $ maybe NoSuchUser checkId mayUsr

  where
    checkId usr | encodeUtf8 (digest $ usr ^. id) == username = Authorized usr
                | otherwise                                  = Unauthorized
