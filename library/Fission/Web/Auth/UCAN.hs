module Fission.Web.Auth.UCAN (handler) where

import           Network.Wai

import           Fission.Prelude
import           Fission.Models

import           Fission.Web.Error.Class
import           Fission.Error.NotFound.Types

import qualified Fission.User.Retriever as User

import qualified Fission.Web.Auth.Token as Token
import qualified Fission.Web.Auth.Error as Auth

import           Fission.Web.Auth.Token.JWT            as JWT
import qualified Fission.Web.Auth.Token.JWT.Validation as JWT
import           Fission.Web.Auth.Token.JWT.Resolver   as Proof

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT.Resolver as JWT

import           Fission.Authorization.Types
import           Fission.Authorization.ServerDID
import           Fission.User.DID.Types





import Servant.Server
import qualified Fission.Web.Error as Web.Error

-- | Auth handler for delegated auth
-- Ensures properly formatted token *and does check against DB*
handler ::
  ( JWT.Resolver     m
  , ServerDID        m
  , MonadLogger      m
  , MonadThrow       m
  , MonadTime        m
  , MonadDB        t m
  , User.Retriever t
  )
  => Request
  -> m Authorization
handler req =
  case Token.get req of
    Just (Token.Bearer (Bearer.Token jwt (Just rawContent))) -> do
      logInfo $ "Incoming request with auth token: " <> rawContent
      void . Web.Error.ensureM =<< JWT.check rawContent jwt
 
      logInfo @Text "Auth token validation success"
      toAuthorization jwt

    _ ->
      Web.Error.throw Auth.NoToken

toAuthorization ::
  ( JWT.Resolver     m
  , MonadThrow       m
  , MonadLogger      m
  , MonadDB        t m
  , User.Retriever t
  )
  => JWT
  -> m Authorization
toAuthorization jwt@JWT {claims = JWT.Claims {..}} = do
  JWT {claims = JWT.Claims {sender = DID {publicKey = pk}}} <- getRoot jwt
  runDB (User.getByPublicKey pk) >>= \case
    Just about -> return Authorization {sender = Right sender, ..}
    Nothing    -> throwM . toServerError $ NotFound @User
 
getRoot :: (JWT.Resolver m, MonadThrow m, MonadLogger m) => JWT -> m JWT
getRoot jwt@JWT {claims = JWT.Claims {proof}} =
  case proof of
    JWT.RootCredential ->
      return jwt

    JWT.Nested _ proofJWT ->
      getRoot proofJWT

    Reference cid ->
      Proof.resolve cid >>= \case
        Right (_, proofJWT) ->
          getRoot proofJWT

        Left err -> do
          logWarn $ "Failed token resolution " <> textDisplay err
          throwM err
