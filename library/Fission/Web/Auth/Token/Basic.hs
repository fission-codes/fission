module Fission.Web.Auth.Token.Basic
  ( handler
  , checkUser
  , parseBasic
  ) where

import qualified Data.ByteString.Char8  as Ch
import qualified Data.ByteString.Base64 as Base64

import           Crypto.BCrypt
import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import qualified Fission.User as User
import           Fission.User.Username.Types

import           Fission.Web.Error

import qualified Fission.Web.Auth.Error as Auth
import qualified Fission.Web.Auth.Token.Basic.Types as Auth.Basic

handler ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadThrow       m
  , MonadDB        t m
  , User.Retriever t
  )
  => Auth.Basic.Token
  -> m (Entity User)
handler token = do
  token
    |> parseBasic
    |> ensureM
    |> bind checkUser
    |> bind ensureM

checkUser ::
  ( MonadLogger      m
  , MonadDB        t m
  , User.Retriever t
  )
  => BasicAuthData
  -> m (Either Auth.Error (Entity User))
checkUser (BasicAuthData username password) = do
  username
    |> decodeUtf8Lenient
    |> Username
    |> User.getByUsername
    |> runDB
    |> bind \case
      Nothing -> do
        logWarn attemptMsg
        return <| Left Auth.NoSuchUser

      Just usr ->
        validate usr

  where
    validate :: MonadLogger m => Entity User -> m (Either Auth.Error (Entity User))
    validate usr@(Entity _ User { userSecretDigest }) =
      case userSecretDigest of
        Just secretDigest ->  do
          if validatePassword (encodeUtf8 secretDigest) password
            then
              return $ Right usr

            else do
              logWarn attemptMsg
              return <| Left Auth.Unauthorized

        Nothing -> do
          logWarn attemptMsg
          return <| Left Auth.Unauthorized

    attemptMsg :: ByteString
    attemptMsg = "Unauthorized user! Attempted with username: " <> username

parseBasic :: Auth.Basic.Token -> Either Auth.Error BasicAuthData
parseBasic (Auth.Basic.Token token) =
  case Ch.split ':' (Base64.decodeLenient token) of
    [un,pw] -> Right $ BasicAuthData un pw
    _ -> Left Auth.BadToken
