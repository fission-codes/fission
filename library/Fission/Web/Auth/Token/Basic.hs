module Fission.Web.Auth.Token.Basic ( handler ) where

import           Fission.Prelude
import           Fission.Models

import           Servant

import           Database.Esqueleto

import           Crypto.BCrypt

-- import qualified Fission.Web.Auth.Types as Auth
import qualified Fission.Web.Auth.Error as Auth

import qualified Fission.User as User

import qualified Data.ByteString.Char8  as Ch
import qualified Data.ByteString.Base64 as Base64

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
  case parseBasic token of
    Left err -> throwM err
    Right auth -> do
      checkUser auth >>= \case
        Left err -> throwM err
        Right usr -> return usr

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
              return <| Right usr

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
    [un,pw] -> Right <| BasicAuthData un pw
    _ -> Left Auth.BadToken
