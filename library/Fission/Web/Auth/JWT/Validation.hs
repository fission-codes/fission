module Fission.Web.Auth.JWT.Validation
  ( check
  , check'
  , checkTime
  , checkSignature
  , checkEd25519Signature
  , checkRSA2048Signature
  ) where

import           Crypto.Error
import           Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.PubKey.Ed25519    as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15 as Crypto.RSA.PKCS

import           Fission.Prelude

import qualified RIO.Text as Text

import qualified Fission.Internal.Base64.Scrubbed as B64.Scrubbed
import qualified Fission.Internal.Crypto          as Crypto

import           Fission.Key  as Key
import qualified Fission.User as User

import           Fission.Web.Auth.JWT.Error as JWT
import           Fission.Web.Auth.JWT.Types as JWT

import           Fission.Web.Auth.JWT.Signature.Types       as Signature
import qualified Fission.Web.Auth.JWT.Signature.RS256.Types as RS256

check :: MonadTime m => ByteString -> JWT -> m (Either JWT.Error JWT)
check rawContent jwt = check' rawContent jwt <$> currentTime

check' :: ByteString -> JWT -> UTCTime -> Either JWT.Error JWT
check' rawContent jwt now = do
  _ <- checkTime now jwt
  checkSignature rawContent jwt

checkTime :: UTCTime -> JWT -> Either JWT.Error JWT
checkTime now jwt@JWT {claims = JWT.Claims { exp, nbf }} = do
  case (now > exp, nbf) of
    (True, _) -> Left JWT.Expired
    (_, Just nbf') -> if now < nbf' then Left JWT.TooEarly else Right jwt
    _ -> Right jwt

checkSignature :: ByteString -> JWT -> Either JWT.Error JWT
checkSignature rawContent jwt@JWT {sig} =
  case sig of
    Signature.Ed25519 _        -> checkEd25519Signature rawContent jwt
    Signature.RS256   rs256Sig -> checkRSA2048Signature rawContent jwt rs256Sig

checkRSA2048Signature :: ByteString -> JWT -> RS256.Signature -> Either JWT.Error JWT
checkRSA2048Signature rawContent jwt@JWT {..} (RS256.Signature innerSig) = do
  case Crypto.decodeToRSA2048PK pk' of
    Left _ ->
      Left BadPublicKey

    Right pk ->
      if Crypto.RSA.PKCS.verify (Just SHA256) pk rawContent innerSig
        then Right jwt
        else Left IncorrectSignature
 
  where
    Claims {iss = User.DID {publicKey = Key.Public pk'}} = claims

checkEd25519Signature :: ByteString -> JWT -> Either JWT.Error JWT
checkEd25519Signature rawContent jwt@JWT {..} =
  case (errOrPk, Crypto.Ed25519.signature sig) of
    (CryptoPassed pk', CryptoPassed sig') ->
      if Crypto.Ed25519.verify pk' rawContent sig'
        then Right jwt
        else Left IncorrectSignature

    (CryptoFailed _, _) ->
      Left BadPublicKey

    (_, CryptoFailed _) ->
      Left BadSignature
    
  where
    errOrPk = Crypto.Ed25519.publicKey $ B64.Scrubbed.scrubB64 pk
    Claims {iss = User.DID {publicKey = Key.Public pk}} = claims
