module Fission.Web.Auth
  ( Checks
  , server
  , basic
  , user
  , checkUser
  , mkAuth
  ) where

import           Crypto.BCrypt
import           Servant

import           Database.Esqueleto

import           Fission.Models
import           Fission.Platform.Heroku as Heroku
import           Fission.Prelude
import qualified Fission.User as User

type Checks = '[BasicAuthCheck (Entity User), BasicAuthCheck ByteString]

-- | Construct an authorization context
mkAuth ::
  ( Heroku.Authorizer m
  , MonadLogger       inner
  , User.Retriever    inner
  )
  => (forall a . inner a -> IO a)
  -> m (Context Checks)
mkAuth runner = do
  herokuAuth <- verify
  return <| user runner
         :. herokuAuth
         :. EmptyContext

server
  :: HasServer api Checks
  => Proxy api
  -> (forall a . m a -> Handler a)
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
  ( User.Retriever m
  , MonadLogger    m
  )
  => (m (BasicAuthResult (Entity User)) -> IO (BasicAuthResult (Entity User)))
  -> BasicAuthCheck (Entity User)
user runner = BasicAuthCheck \auth ->
  auth
    |> checkUser
    |> runner

checkUser ::
  -- ( User.Queryable m
  (
    User.Retriever m
  -- , MonadDB           m
  , MonadLogger    m
  )
  => BasicAuthData
  -> m (BasicAuthResult (Entity User))
checkUser (BasicAuthData username password) = do
  mayUser <- do
    username
      |> decodeUtf8Lenient
      |> User.getByUsername

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
