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

server
  :: HasServer api '[BasicAuthCheck User, BasicAuthCheck ByteString]
  => Proxy api
  -> cfg
  -> RIOServer cfg api
  -> Server api
server api cfg = hoistServerWithContext api context (toHandler cfg)

context :: Proxy (BasicAuthCheck User ': BasicAuthCheck ByteString ': '[])
context = Proxy

basic :: ByteString -> ByteString -> BasicAuthCheck ByteString
basic unOK pwOK = BasicAuthCheck (pure . check')
  where
    check' :: BasicAuthData -> BasicAuthResult ByteString
    check' (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then Authorized username
         else Unauthorized

user :: (MonadRIO cfg m, HasLogFunc cfg, MonadSelda (RIO cfg)) => m (BasicAuthCheck User)
user = do
  cfg <- ask
  return <| BasicAuthCheck \auth -> runRIO cfg <| checkUser auth



-- TODO: Should we do a case-insensitive query for username instead?
--
checkUser
  :: MonadDatabase
  => BasicAuthData
  -> RIO cfg (BasicAuthResult User)

checkUser (BasicAuthData authUsername authPassword) = do
  (\u -> do
    where_ (byUsername u)
    return u
  )
    |> from
    |> selectFirst
    |> andThen (
      maybe
        (pure NoSuchUser)
        checkPassword
    )

  where
    byUsername user =
      (user ^. active ==. True) ++
      (user ^. username ==. decodeUtf8Lenient authUsername)

    checkPassword :: (MonadRIO cfg m, HasLogFunc cfg) => User -> m (BasicAuthResult User)
    checkPassword usr =
      if validatePassword (encodeUtf8 <| secretDigest <| usr) password
         then
           return (Authorized usr)

         else do
           logWarn <| "Unauthorized user! Username: " <> displayBytesUtf8 username
           return Unauthorized
