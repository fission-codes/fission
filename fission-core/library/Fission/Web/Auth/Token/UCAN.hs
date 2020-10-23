module Fission.Web.Auth.Token.UCAN
  ( toAuthorization
  -- , getRoot
  ) where

import qualified System.IO.Unsafe                                 as Unsafe

import           Fission.Web.Auth.Token.UCAN.Attenuated.Types
import           Fission.Web.Auth.Token.UCAN.Privilege.Types


import           Fission.Web.Auth.Token.UCAN.Proof.Types


import qualified Fission.Web.Auth.Token.UCAN.Proof.Types          as Proof


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


-- import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Prelude

import           Fission.Models

import           Fission.Error.NotFound.Types
import qualified Fission.User.Retriever                           as User
import qualified Fission.Web.Error                                as Web.Error

-- import           Fission.Web.Auth.Token.JWT                       as JWT
import           Fission.Web.Auth.Token.JWT.Resolver              as Proof
import qualified Fission.Web.Auth.Token.JWT.Validation            as JWT

-- import qualified Fission.Web.Auth.Token.Bearer.Types              as Bearer
import           Fission.Web.Auth.Token.JWT.Resolver              as JWT

import           Fission.Authorization.ServerDID
import           Fission.Authorization.Types
import           Fission.User.DID.Types
import           Fission.User.Username.Types                      (Username)
-- import           Fission.Web.Auth.Token.JWT
-- import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import           Fission.Web.Auth.Token.UCAN.Types                as UCAN

import qualified Fission.WNFS.Capability.Types                    as WNFS
import qualified Fission.WNFS.Privilege.Types                     as WNFS
import qualified Fission.WNFS.Subgraph.Types                      as WNFS




import           Fission.Web.Auth.Token.JWT.Resolver.Class        as UCAN

toAuthorization ::
  forall m t privilege fact .
  ( m `Proof.Resolves` UCAN privilege fact
  -- , MonadThrow       m
  -- , MonadLogger      m
  , MonadDB        t m
  , User.Retriever t
  )
  => UCAN privilege fact -- FIXME would sure be nice to have a typesafe resolved version
  -> m (Either JWT.Error (Authorization privilege)) -- FIXME couldn't hurt to make this more structured
toAuthorization ucan@UCAN {claims = UCAN.Claims {..}} = do
  toAccesses ucan >>= \case
    Left  err    -> return $ Left err
    Right access -> return $ Right Authorization {sender = Right sender, access}

toAccesses ::
  ( m `Proof.Resolves` UCAN privilege fact
  , MonadDB        t m
  , User.Retriever t
  )
  => UCAN privilege fact
  -> m (Either JWT.Error [Access privilege])
toAccesses UCAN {attenuations = Subset privs}} =
  return $ Right (Specifically <$> privs)

toAccesses UCAN {claims = UCAN.Claims {sender = DID {publicKey}, attenuations = AllInScope, proofs}} =
  case proofs of
    RootCredential -> do
      runDB (User.getByPublicKey publicKey) >>= \case
        Nothing ->
          undefined -- FIXME

        Just rootUser ->
          return $ Right [AllBelongingTo rootUser]

    DelegatedFrom delegateProofs ->
      foldM folder (Right []) delegateProofs

  where
    folder ::
         Either JWT.Error [Access privilege]
      -> DelegateProof (UCAN privilege fact)
      -> m (Either JWT.Error [Access privilege])
    folder (Left err) _ =
      return $ Left err

    folder (Right acc) (Nested _ proof) =
      appendAccesses acc proof

    folder (Right acc) (Reference cid) =
      UCAN.resolve cid >>= \case
        Left  err       -> return $ Left err
        Right (_, ucan) -> appendAccesses acc ucan

    appendAccesses :: [Access privilege] -> UCAN privilege fact -> [Access privilege]
    appendAccesses acc ucan =
      toAccesses ucan >>= \case
        Left err -> return $ Left err
        Right newPrivs -> return $ Right (acc <> newPrivs)
