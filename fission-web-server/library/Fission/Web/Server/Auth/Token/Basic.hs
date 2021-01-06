module Fission.Web.Server.Auth.Token.Basic
  ( handler
  , checkUser
  , parseBasic
  ) where

import qualified Data.ByteString.Base64                                  as Base64
import qualified Data.ByteString.Char8                                   as Ch

import           Crypto.BCrypt
import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import           Fission.Authorization
import           Fission.User.Username.Types                             as Username

import           Fission.Web.Server.Error                                as Web.Err
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB
import qualified Fission.Web.Server.User                                 as User

import qualified Fission.Web.Server.Auth.Error                           as Auth
import qualified Fission.Web.Server.Auth.Token.Basic.Types               as Auth.Basic

import           Fission.Web.Server.Auth.Token.UCAN.Resource.Scope.Types

handler ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadThrow       m
  , MonadDB        t m
  , User.Retriever t
  )
  => Auth.Basic.Token
  -> m Authorization
handler token = Web.Err.ensureM . checkUser =<< Web.Err.ensure (parseBasic token)

checkUser ::
  ( MonadLogger      m
  , MonadDB        t m
  , User.Retriever t
  )
  => BasicAuthData
  -> m (Either Auth.Error Authorization)
checkUser (BasicAuthData rawUsername password) =
  case mkUsername $ decodeUtf8Lenient rawUsername of
    Left _ -> do
      logWarn attemptMsg
      return $ Left Auth.NoSuchUser

    Right username ->
      runDB (User.getByUsername username) >>= \case
        Nothing -> do
          logWarn attemptMsg
          return $ Left Auth.NoSuchUser

        Just usr ->
          validate usr

  where
    validate :: MonadLogger m => Entity User -> m (Either Auth.Error Authorization)
    validate user@(Entity _ User { userSecretDigest }) =
      case userSecretDigest of
        Just secretDigest ->  do
          if validatePassword (encodeUtf8 secretDigest) password
            then
              return $ Right Authorization
                { about    = user
                , sender   = Left Heroku
                , potency  = SuperUser
                , resource = Complete
                }

            else do
              logWarn attemptMsg
              return $ Left Auth.Unauthorized

        Nothing -> do
          logWarn attemptMsg
          return $ Left Auth.Unauthorized

    attemptMsg :: ByteString
    attemptMsg = "Unauthorized user! Attempted with username: " <> rawUsername

parseBasic :: Auth.Basic.Token -> Either Auth.Error BasicAuthData
parseBasic (Auth.Basic.Token token) =
  case Ch.split ':' (Base64.decodeBase64Lenient token) of
    [un,pw] -> Right $ BasicAuthData un pw
    _       -> Left Auth.NoToken
