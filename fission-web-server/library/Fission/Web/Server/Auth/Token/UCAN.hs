module Fission.Web.Server.Auth.Token.UCAN (handler) where

import           Web.DID.Types                                      as DID
import           Web.UCAN.Resolver                                  as UCAN
import qualified Web.UCAN.Types                                     as UCAN
import qualified Web.UCAN.Validation                                as UCAN

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.Authorization.ServerDID

import qualified Fission.Web.Auth.Token.Bearer.Types                as Bearer
import qualified Fission.Web.Auth.Token.UCAN                        as UCAN
import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Types

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
  void . Web.Error.ensureM $ UCAN.check serverDID rawContent jwt
  toAuthorization jwt

toAuthorization ::
  ( UCAN.Resolver    m
  , MonadThrow       m
  , MonadLogger      m
  , MonadDB        t m
  , User.Retriever t
  )
  => UCAN
  -> m Authorization
toAuthorization jwt@UCAN.UCAN {claims = UCAN.Claims {..}} = do
  logDebug @Text "🛂 Authorizing UCAN..."
  UCAN.getRoot jwt >>= \case
    Left err ->
      throwM err

    Right UCAN.UCAN {claims = UCAN.Claims {sender = DID.Key pk}} ->
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
