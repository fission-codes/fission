module Fission.Web.Auth.Token.JWT
  ( getRoot
  , getRootDID
  , mkUCAN
  , simpleWNFS
  , module Fission.Web.Auth.Token.JWT.Types
  ) where

-- import qualified RIO.ByteString.Lazy                              as Lazy
-- import qualified RIO.Text                                         as Text

import qualified Crypto.PubKey.Ed25519                            as Ed25519

import           Fission.Prelude

import qualified Fission.Internal.Base64.URL                      as B64.URL

import qualified Fission.Key                                      as Key
import           Fission.User.DID                                 as DID

import           Fission.Authorization                            as Authorization
import           Fission.Key.Asymmetric.Algorithm.Types           as Key

import qualified Fission.Key.Asymmetric.Public.Types              as Asymmetric

import           Fission.Web.Auth.Token.UCAN.Resource.Types

-- import qualified Fission.Web.Auth.Token.Bearer.Types              as Bearer
import           Fission.Web.Auth.Token.JWT.Fact.Types
import qualified Fission.Web.Auth.Token.JWT.Header.Typ.Types      as JWT.Typ
-- import qualified Fission.Web.Auth.Token.JWT.Proof                 as JWT.Proof
-- import           Fission.Web.Auth.Token.JWT.Resolver              as JWT.Resolver
import qualified Fission.Web.Auth.Token.JWT.Resolver              as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class        as Proof
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error        as Resolver
import qualified Fission.Web.Auth.Token.JWT.Signature.Types       as JWT.Signature
import           Fission.Web.Auth.Token.JWT.Types                 as JWT
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

-- Reexports

import           Fission.Web.Auth.Token.JWT.Types

getRoot :: JWT.Resolver m => JWT -> m (Either Resolver.Error JWT)
getRoot jwt@JWT {claims = Claims {proof}} =
  case proof of
    RootCredential ->
      return $ Right jwt

    Nested _ proofJWT ->
      getRoot proofJWT

    Reference cid ->
      Proof.resolve cid >>= \case
        Right (_, proofJWT) -> getRoot proofJWT
        Left err            -> return $ Left err

getRootDID ::
  ( JWT.Resolver m
  , MonadRaise   m
  , m `Raises` Resolver.Error
  )
  => Asymmetric.Public
  -> Proof
  -> m DID
getRootDID fallbackPK = \case
  RootCredential ->
    return $ DID Key fallbackPK

  Nested _ jwt ->  do
    JWT {claims = JWT.Claims {sender}} <- ensureM $ getRoot jwt
    return sender

  Reference cid -> do
    (_, JWT {claims = JWT.Claims {proof}}) <- ensureM $ Proof.resolve cid
    getRootDID fallbackPK proof

-- FIXME may be able to move to inside the web client or CLI
simpleWNFS :: UTCTime -> DID -> Ed25519.SecretKey -> [Fact] -> Proof -> JWT
simpleWNFS now fissionDID sk facts proof =
  mkUCAN fissionDID sk begin expiry facts resource potency proof
  where
    potency  = AppendOnly
    resource = Just (Subset (FissionFileSystem "/"))

    -- Accounting for clock drift
    begin  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expiry = addUTCTime (secondsToNominalDiffTime   30)  now

mkUCAN ::
     DID
  -> Ed25519.SecretKey
  -> UTCTime
  -> UTCTime
  -> [Fact]
  -> Maybe (Scope Resource)
  -> Potency
  -> Proof
  -> JWT
mkUCAN receiver senderSK nbf exp facts resource potency proof =
  JWT {..}
  where
    sig = JWT.Signature.Ed25519 . Key.signWith senderSK . encodeUtf8 $ B64.URL.encodeJWT header claims

    sender = DID
      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic senderSK
      , method    = DID.Key
      }

    claims = JWT.Claims {..}

    header = JWT.Header
      { typ = JWT.Typ.JWT
      , alg = Key.Ed25519
      , cty = Nothing
      , uav = Authorization.latestVersion
      }
