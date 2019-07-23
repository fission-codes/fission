module Fission.Web.Auth
  ( server
  , context
  , basic
  , user
  , checkUser
  ) where

import RIO

import Database.Selda
import Servant

import Fission.Internal.Orphanage ()
import Fission.Internal.Constraint

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

user :: MonadRIO        cfg m
     => HasLogFunc      cfg
     => MonadSelda (RIO cfg)
     => m (BasicAuthCheck User)
user = do
  cfg <- ask
  return $ BasicAuthCheck (runRIO cfg . checkUser)

checkUser :: HasLogFunc cfg
          => MonadSelda (RIO cfg)
          => BasicAuthData
          -> RIO cfg (BasicAuthResult User)
checkUser (BasicAuthData username password) = do
  mayUser <- undefined -- getOne
          -- . query
          -- . limit 0 1
          -- $ select User.users `suchThat` User.bySecret (decodeUtf8Lenient password)

  -- maybe (pure NoSuchUser) checkID mayUser
  undefined

  -- where
  --   checkID :: (MonadRIO cfg m, HasLogFunc cfg) => User -> m (BasicAuthResult User)
  --   checkID usr =
  --     if encodeUtf8 (hashID $ usr ^. userID) == username
  --        then return (Authorized usr)
  --        else do
  --          logWarn $ mconcat
  --            [ "Unauthorized user! HashedID: "
  --            , displayBytesUtf8 username
  --            , ", secret: "
  --            , displayBytesUtf8 password
  --            ]

  --          return Unauthorized
