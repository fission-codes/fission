module Fission.Web.Auth
  ( Checks
  , server
  , basic
  , user
  , checkUser
  ) where

import           Crypto.BCrypt
import           Servant

import           Database.Esqueleto
import qualified Database.Persist as P

import           Fission.Models
import           Fission.Prelude

type Checks = '[BasicAuthCheck (Entity User), BasicAuthCheck ByteString]

server
  :: HasServer api Checks
  => Proxy api
  -> (forall a . m a -> Handler a) -- This is an existential type; basically `a` is locally scoped to that function at call time
  -> ServerT api m
  -> ServerT api Handler
server api = hoistServerWithContext api context
  where
    context :: Proxy Checks
    context = Proxy

basic :: ByteString -> ByteString -> BasicAuthCheck ByteString
basic unOK pwOK = BasicAuthCheck (pure . check')
  where
    check' :: BasicAuthData -> BasicAuthResult ByteString
    check' (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then Authorized username
         else Unauthorized

user ::
  ( MonadDB     (RIO cfg)
  , MonadLogger (RIO cfg)
  )
  => cfg
  -> BasicAuthCheck (Entity User)
user cfg = BasicAuthCheck \auth -> runRIO cfg <| checkUser auth

checkUser ::
  ( MonadDB     m
  , MonadLogger m
  )
  => BasicAuthData
  -> m (BasicAuthResult (Entity User))
checkUser (BasicAuthData username password) = do
  mayUser <- runDB <| selectFirst
    [ UserUsername P.==. decodeUtf8Lenient username
    , UserActive   P.==. True
    ] []

  case mayUser of
    Nothing -> do
      logWarn attemptMsg
      return NoSuchUser

    Just usr ->
      validate usr

  where
    validate :: MonadLogger m => Entity User -> m (BasicAuthResult (Entity User))
    validate usr@(Entity _ User { userSecretDigest }) =
      if validatePassword (encodeUtf8 userSecretDigest) password
         then
           return (Authorized usr)

         else do
           logWarn attemptMsg
           return Unauthorized

    attemptMsg :: ByteString
    attemptMsg = "Unauthorized user! Attempted with username: " <> username
