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
import Database.Selda
import Database.Esqueleto
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


-- TODO: Should we do a case-insensitive query for username instead?
--
checkUser
  :: BasicAuthData
  -> RIO cfg (BasicAuthResult User)

checkUser (BasicAuthData authUsername authPassword) = do
  maybeUser <- selectFirst <| from \user -> do
    where_ (byUsername user)
    return user

  maybe
    (pure NoSuchUser)
    checkPassword
    maybeUser

  where
    byUsername user =
      (user ^. active ==. True) ++
      (user ^. username ==. decodeUtf8Lenient authUsername)

    checkPassword usr =
      if validatePassword (encodeUtf8 <| secretDigest <| usr) password then
        return (Authorized usr)

      else do
        logWarn ("Unauthorized user! Username: " <> displayBytesUtf8 username)
        return Unauthorized
