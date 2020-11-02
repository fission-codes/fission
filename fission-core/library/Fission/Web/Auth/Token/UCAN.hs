module Fission.Web.Auth.Token.UCAN
  ( handler
  , getRoot
  ) where

import           Fission.Models
import           Fission.Prelude

import           Fission.Error.NotFound.Types
import qualified Fission.User.Retriever                    as User
import qualified Fission.Web.Error                         as Web.Error

import           Fission.Web.Auth.Token.JWT                as JWT
import           Fission.Web.Auth.Token.JWT.Resolver       as Proof
import qualified Fission.Web.Auth.Token.JWT.Validation     as JWT

import qualified Fission.Web.Auth.Token.Bearer.Types       as Bearer
import           Fission.Web.Auth.Token.JWT.Resolver       as JWT

import           Fission.Authorization.ServerDID
import           Fission.Authorization.Types
import           Fission.User.DID.Types

import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as Resolver

-- | Auth handler for delegated auth
-- Ensures properly formatted token *and does check against DB*
handler ::
  ( MonadLogger      m
  , MonadThrow       m
  , Resolver         m
  , ServerDID        m
  , MonadTime        m
  , MonadDB        t m
  , User.Retriever t
  )
  => Bearer.Token
  -> m Authorization
handler (Bearer.Token jwt rawContent) = do
  void . Web.Error.ensureM $ JWT.check rawContent jwt
  toAuthorization jwt

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
  getRoot jwt >>= \case
    Left err ->
      throwM err

    Right JWT {claims = JWT.Claims {sender = DID {publicKey = pk}}} ->
      runDB (User.getByPublicKey pk) >>= \case
        Just about -> return Authorization {sender = Right sender, ..}
        Nothing    -> Web.Error.throw $ NotFound @User

-- FIXME move to better module
getRoot ::
  ( JWT.Resolver m
  , MonadLogger  m
  )
  => JWT
  -> m (Either Resolver.Error JWT)
getRoot jwt@JWT {claims = JWT.Claims {proof}} =
  case proof of
    JWT.RootCredential ->
      return $ Right jwt

    JWT.Nested _ proofJWT ->
      getRoot proofJWT

    Reference cid ->
      Proof.resolve cid >>= \case
        Right (_, proofJWT) ->
          getRoot proofJWT

        Left err -> do
          logWarn $ "Failed token resolution " <> textDisplay err
          return $ Left err
