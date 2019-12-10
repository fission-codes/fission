module Fission.Web.Auth
  ( ExistingUser
  , HerokuAddOnAPI
  , server
  , context
  , basic
  , user
  , checkUser
  ) where

import Crypto.BCrypt
import Database.Esqueleto
import qualified RIO.Text
import Servant

import Fission.Prelude
import Fission.Storage.Query
import Fission.User as User hiding (username)
import Fission.Web.Server


type ExistingUser = BasicAuth "existing user" User
type HerokuAddOnAPI = BasicAuth "heroku add-on api" ByteString



-- SERVER & CONTEXT


server
  :: HasServer api '[BasicAuthCheck User, BasicAuthCheck ByteString]
  => Proxy api
  -> cfg
  -> RIOServer cfg api
  -> Server api
server api cfg =
  hoistServerWithContext api context (toHandler cfg)


context :: Proxy (BasicAuthCheck User ': BasicAuthCheck ByteString ': '[])
context = Proxy



-- BASIC AUTH


basic :: ByteString -> ByteString -> BasicAuthCheck ByteString
basic unOK pwOK = BasicAuthCheck (pure . check')
  where
    check' :: BasicAuthData -> BasicAuthResult ByteString
    check' (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then Authorized username
         else Unauthorized


user :: ( MonadDatabase (RIO cfg)
        , MonadRIO cfg m
        , HasLogFunc cfg
        )
=> m (BasicAuthCheck User)
user = do
  cfg <- ask
  return (BasicAuthCheck \auth -> runRIO cfg <| checkUser auth)


checkUser
  :: BasicAuthData
  -> RIO cfg (BasicAuthResult User)
checkUser (BasicAuthData username password) = do
  maybeUser <- Query.oneWhere byUsername

  maybe
    NoSuchUser
    checkPassword
    maybeUser

  where
    lowerCaseUsername =
      Text.toLower username

    byUsername user =
      (user ^. active ==. True) ++
      (user ^. (Query.lower_ username) ==. decodeUtf8Lenient lowerCaseUsername)



-- ㊙️


checkPassword : User -> BasicAuthResult User
checkPassword User { secretDigest } =
  if validatePassword (encodeUtf8 secretDigest) password then
    Authorized usr

  else do
    logWarn ("Unauthorized user! Username: " <> displayBytesUtf8 username)
    Unauthorized
