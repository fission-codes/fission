module Fission.Web.Auth
  ( server
  , context
  , basic
  , user
  , checkUser
  -- , heroku
  ) where

import RIO

import Database.Selda
import Servant

import Fission.Internal.Orphanage ()
import Fission.Storage.Query
import Fission.User as User
import Fission.Web.Server

server :: HasServer api '[BasicAuthCheck User, BasicAuthCheck ByteString]
       => Proxy api
       -> cfg
       -> RIOServer cfg api
       -> Server api
server api cfg = hoistServerWithContext api context (toHandler cfg)

context :: Proxy (BasicAuthCheck User ': BasicAuthCheck ByteString ': '[])
context = Proxy

basic :: ByteString
      -> ByteString
      -> BasicAuthCheck ByteString
basic unOK pwOK = BasicAuthCheck (pure . check')
  where
    check' :: BasicAuthData -> BasicAuthResult ByteString
    check' (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then Authorized username
         else Unauthorized

user :: HasLogFunc cfg
     => MonadSelda (RIO cfg)
     => RIO cfg (BasicAuthCheck User)
user = do
  cfg <- ask
  return $ BasicAuthCheck (runRIO cfg . checkUser)

checkUser :: HasLogFunc cfg
          => MonadSelda (RIO cfg)
          => BasicAuthData
          -> RIO cfg (BasicAuthResult User)
checkUser (BasicAuthData username password) = do
  mayUsr <- getOne . User.bySecret $ decodeUtf8Lenient password
  maybe (return NoSuchUser) checkID mayUsr

  where
    checkID usr =
      if encodeUtf8 (hashID $ usr ^. userID) == username
         then return (Authorized usr)
         else do
           logWarn $ mconcat
             [ "Unauthorized user! HashedID: "
             , displayBytesUtf8 username
             , ", secret: "
             , displayBytesUtf8 password
             ]

           return Unauthorized
