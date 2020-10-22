module Fission.Web.Auth.Token.UCAN (handler) where


import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Prelude

import           Fission.Models

import           Fission.Error.NotFound.Types
import qualified Fission.User.Retriever                           as User
import qualified Fission.Web.Error                                as Web.Error

import           Fission.Web.Auth.Token.JWT                       as JWT
import           Fission.Web.Auth.Token.JWT.Resolver              as Proof
import qualified Fission.Web.Auth.Token.JWT.Validation            as JWT

import qualified Fission.Web.Auth.Token.Bearer.Types              as Bearer
import           Fission.Web.Auth.Token.JWT.Resolver              as JWT

import           Fission.Authorization.ServerDID
import           Fission.Authorization.Types
import           Fission.User.DID.Types
import           Fission.User.Username.Types                      (Username)
import           Fission.Web.Auth.Token.JWT
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

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
  -> m (Authorization [Resource])
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
  => UCAN
  -> m (Authorization [Resource])
toAuthorization ucan@UCAN {claims = JWT.Claims {..}} = do
  UCAN {claims = JWT.Claims {sender = DID {publicKey = pk}}} <- getRoot ucan
  runDB (User.getByPublicKey pk) >>= \case
    Just about@(Entity _ User {userUsername}) -> return Authorization {sender = Right sender, rights = toRights userUsername attenuations, about}
    Nothing    -> Web.Error.throw $ NotFound @User


toRights :: Username -> Scope [Resource] -> [Resource] -- FIXME RENAME "Attenuation" to "Right"
toRights name = \case
  Subset atts ->
    atts

  Complete ->
    -- Default WNFS rights
    [ FissionFileSystem WNFSAttenuation -- FIXME Not on Resource!
        { wnfsResource = WNFSResource
            { namespace = "fission.name"
            , username  = name
            , filePath  = "/public/"
            }
        , capability = SuperUser
        }
    , FissionFileSystem WNFSAttenuation
        { wnfsResource = WNFSResource
            { namespace = "fission.name"
            , username  = name
            , filePath  = "/private/"
            }
        , capability = SuperUser
        }
    ]


getRoot :: (JWT.Resolver m, MonadThrow m, MonadLogger m) => UCAN -> m UCAN
getRoot ucan@UCAN {claims = JWT.Claims {proofs}} =
  case proofs of
    JWT.RootCredential ->
      return ucan

-- FIXME new structure
    JWT.Nested _ proofJWT ->
      getRoot proofJWT

    Reference cid ->
      Proof.resolve cid >>= \case
        Right (_, proofJWT) ->
          getRoot proofJWT

        Left err -> do
          logWarn $ "Failed token resolution " <> textDisplay err
          throwM err
