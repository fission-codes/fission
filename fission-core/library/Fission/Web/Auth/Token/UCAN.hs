module Fission.Web.Auth.Token.UCAN (handler) where

import qualified System.IO.Unsafe                                 as Unsafe

import           Fission.Web.Auth.Token.UCAN.Attenuated.Types


import           Fission.Web.Auth.Token.UCAN.Proof.Types




import           Crypto.Hash.Algorithms                           (SHA256 (..))
import           Crypto.Random                                    (MonadRandom (..))

import qualified Crypto.PubKey.RSA                                as RSA
import qualified Crypto.PubKey.RSA.PKCS15                         as RSA.PKCS15

import           Crypto.PubKey.Ed25519                            (toPublic)
import qualified Crypto.PubKey.Ed25519                            as Ed25519

import qualified Data.ByteString.Base64.URL                       as BS.B64.URL

import qualified RIO.ByteString.Lazy                              as Lazy
import qualified RIO.Text                                         as Text

import qualified Fission.Internal.Base64.URL                      as B64.URL
import           Fission.Prelude

import qualified Fission.Key.Asymmetric.Algorithm.Types           as Algorithm

import qualified Fission.Internal.RSA2048.Pair.Types              as RSA2048
import qualified Fission.Internal.UTF8                            as UTF8

import           Fission.Key                                      as Key

import           Fission.User.DID.Types

import           Fission.Web.Auth.Token.JWT.Header.Types          (Header (..))
import           Fission.Web.Auth.Token.JWT.Signature             as Signature
import qualified Fission.Web.Auth.Token.JWT.Signature.RS256.Types as RS256

-- Reexports

import           Fission.Web.Auth.Token.JWT.RawContent

-- Orphans

import           Fission.Internal.Orphanage.CID                   ()
import           Fission.Internal.Orphanage.Ed25519.SecretKey     ()


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


toRights :: Username -> Attenuated [Resource] -> [Resource] -- FIXME RENAME "Attenuation" to "Right"
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

signEd25519 ::
  ToJSON claims
  => Header
  -> claims
  -> Ed25519.SecretKey
  -> Signature.Signature
signEd25519 header claims sk =
  Signature.Ed25519 . Key.signWith sk . encodeUtf8 $ B64.URL.encodeJWT header claims

signRS256 ::
  ( ToJSON claims
  , MonadRandom m
  )
  => Header
  -> claims
  -> RSA.PrivateKey
  -> m (Either RSA.Error Signature.Signature)
signRS256 header claims sk =
  RSA.PKCS15.signSafer (Just SHA256) sk (encodeUtf8 $ B64.URL.encodeJWT header claims) <&> \case
    Left err  -> Left err
    Right sig -> Right . Signature.RS256 $ RS256.Signature sig
