module Fission.Web.Auth
  ( ExistingUser
  , HerokuAddOnAPI
  , server
  , context
  , basic
  , user
  , checkUser
  ) where

import RIO

import Database.Selda
import Servant

import Crypto.BCrypt

import Fission.Internal.Constraint
import Fission.Storage.Query
import Fission.User as User hiding (username)
import Fission.Web.Server

type ExistingUser = BasicAuth "existing user" User
type HerokuAddOnAPI = BasicAuth "heroku add-on api" ByteString

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
  mayUser <- findOne $ select User.users `suchThat` User.byUsername (decodeUtf8Lenient username)
  maybe (pure NoSuchUser) checkPassword mayUser

  where
    checkPassword :: (MonadRIO cfg m, HasLogFunc cfg) => User -> m (BasicAuthResult User)
    checkPassword usr =
      if validatePassword (encodeUtf8 $ usr ^. secretDigest) password
         then return (Authorized usr)
         else do
           logWarn $ mconcat
             [ "Unauthorized user! Username: "
             , displayBytesUtf8 username
             ]

           return Unauthorized
