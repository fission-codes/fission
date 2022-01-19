module Fission.Web.Server.Auth.Token.UCAN (handler) where

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.Authorization.ServerDID
import           Web.DID.Types

import           Fission.Web.Auth.Token.JWT                         as JWT
import           Fission.Web.Auth.Token.JWT.Resolver                as Proof
import qualified Fission.Web.Auth.Token.JWT.Validation              as JWT

import qualified Fission.Web.Auth.Token.Bearer.Types                as Bearer
import           Fission.Web.Auth.Token.JWT.Resolver                as JWT

import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error                           as Web.Error
import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB
import qualified Fission.Web.Server.User.Retriever                  as User

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
  serverDID <- getServerDID
  void . Web.Error.ensureM $ JWT.check serverDID rawContent jwt
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
  logDebug @Text "🛂 Authorizing UCAN..."
  getRoot jwt >>= \case
    Left err ->
      throwM err

    Right JWT {claims = JWT.Claims {sender = DID {publicKey = pk}}} ->
      runDB (User.getByPublicKey pk) >>= \case
        Nothing ->
          Web.Error.throw $ NotFound @User

        Just about@(Entity userId _) ->
          case resource of
            Nothing  -> Web.Error.throw $ ActionNotAuthorized @Resource userId
            Just res -> return Authorization { sender   = Right sender
                                             , resource = res
                                             , ..
                                             }
