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

import RIO hiding (id)

import Servant
import Database.Selda

import Fission.Internal.Orphanage ()
import Fission.Internal.Constraint

import Fission.Security
import Fission.Web.Server
import Fission.User as User
import Fission.Storage.SQLite

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

user :: MonadSelda IO => Context (BasicAuthCheck User ': '[])
user = BasicAuthCheck check :. EmptyContext

check' :: (MonadRIO m cfg, MonadSelda IO) => RIO cfg (BasicAuthResult User)
check' = ioToRIO . check

ioToRIO :: IO a -> RIO cfg a
ioToRIO = liftIO

check :: MonadIO io => MonadSelda io => BasicAuthData -> io (BasicAuthResult User)
check (BasicAuthData username password) = do
  mayUsr <- getOne . User.bySecret $ decodeUtf8Lenient password
  return $ maybe NoSuchUser checkId mayUsr

  where
    checkId usr | encodeUtf8 (digest $ usr ^. id) == username = Authorized usr
                | otherwise                                  = Unauthorized
