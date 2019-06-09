module Fission.Web.Auth
  ( server
  , context
  , basic
  , user
  , checkUser
  ) where

import RIO

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
basic unOK pwOK = BasicAuthCheck (pure . check') :. EmptyContext
  where
    check' :: BasicAuthData -> BasicAuthResult ByteString
    check' (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then Authorized username
         else Unauthorized

user :: MonadSelda (RIO cfg) => RIO cfg (Context (BasicAuthCheck User ': '[]))
user = do
  cfg <- ask
  return $ BasicAuthCheck (runRIO cfg . checkUser) :. EmptyContext

checkUser :: MonadSelda (RIO cfg) => BasicAuthData -> RIO cfg (BasicAuthResult User)
checkUser (BasicAuthData username password) = do
  mayUsr <- getOne . User.bySecret $ decodeUtf8Lenient password
  return $ maybe NoSuchUser checkID mayUsr

  where
    checkID :: User -> BasicAuthResult User
    checkID usr =
      if encodeUtf8 (digest $ usr ^. userID) == username
         then Authorized usr
         else Unauthorized
