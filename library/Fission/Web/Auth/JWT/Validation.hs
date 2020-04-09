module Fission.Web.Auth.JWT.Validation
  ( check
  , check'
  , checkTime
  , checkSignature
  , checkEd25519Signature
  , checkRSA2048Signature
  ) where

import qualified Data.Binary as Binary

import           Crypto.Error
import           Crypto.Hash.Algorithms (SHA256 (..))
import qualified Codec.Crypto.RSA.Pure as Codec.RSA

import qualified Crypto.PubKey.Ed25519    as Crypto.Ed25519
import qualified Crypto.PubKey.RSA        as Crypto.RSA
import qualified Crypto.PubKey.RSA.PKCS15 as Crypto.RSA.PKCS

import qualified RIO.ByteString.Lazy as Lazy

import           Fission.Prelude
import qualified Fission.Internal.UTF8            as UTF8
import qualified Fission.Internal.Base64.Scrubbed as B64.Scrubbed

import           Fission.Key  as Key
import qualified Fission.User as User

import           Fission.Web.Auth.JWT.Error as JWT
import           Fission.Web.Auth.JWT.Types as JWT

import           Fission.Web.Auth.JWT.Signature.Types       as Signature
import qualified Fission.Web.Auth.JWT.Signature.RS256.Types as RS256







import qualified RIO.Text as Text

import qualified Codec.Crypto.RSA.Pure as RSA

import qualified Fission.Internal.Crypto as Crypto
import qualified Fission.Internal.Base64 as B64
import qualified Fission.Internal.Base64.URL as B64.URL

check :: MonadTime m => JWT -> m (Either JWT.Error JWT)
check jwt = check' jwt <$> currentTime

check' :: JWT -> UTCTime -> Either JWT.Error JWT
check' jwt now =
  pure jwt
    >>= checkTime now
    >>= checkSignature

checkTime :: UTCTime -> JWT -> Either JWT.Error JWT
checkTime now jwt@JWT {claims = JWT.Claims { exp, nbf }} = do
  case (now > exp, nbf) of
    (True, _) -> Left JWT.Expired
    (_, Just nbf') -> if now < nbf' then Left JWT.TooEarly else Right jwt
    _ -> Right jwt

checkSignature :: JWT -> Either JWT.Error JWT
checkSignature jwt@JWT {sig} =
  case sig of
    Signature.Ed25519 _        -> checkEd25519Signature jwt
    Signature.RS256   rs256Sig -> checkRSA2048Signature jwt rs256Sig
 
checkRSA2048Signature :: JWT -> RS256.Signature -> Either JWT.Error JWT
checkRSA2048Signature (jwt@JWT {..}) (RS256.Signature innerSig) = do
  if Crypto.RSA.PKCS.verify (Nothing :: Maybe SHA256) pk content generatedSigB64
    then Right jwt
    else Left IncorrectSignature
 
  where
    content =
      (encodeUtf8 $ B64.URL.encode $ decodeUtf8Lenient $ B64.toB64ByteString $ Lazy.toStrict (encode header)) <> "."
        <> (encodeUtf8 $ B64.URL.encode $ decodeUtf8Lenient $ B64.toB64ByteString $ Lazy.toStrict $ encode claims)

    pk = Crypto.decodeToRSA2048Pk innerSig
    Claims {iss = User.DID {publicKey = Key.Public pk'}}  = claims

-- instance Binary PublicKey where
--   put pk = do sizeBS <- failOnError (i2osp (public_size pk) 8)
--               nBS <- failOnError (i2osp (public_n pk) (public_size pk))
--               putLazyByteString sizeBS
--               putLazyByteString nBS
--   get    = do len <- (fromIntegral . os2ip) `fmap` getLazyByteString 8
--               n   <- os2ip `fmap` getLazyByteString len
--               return (PublicKey (fromIntegral len) n 65537)

-----------------------------

checkEd25519Signature :: JWT -> Either JWT.Error JWT
checkEd25519Signature jwt@JWT {..} =
  case (errOrPk, Crypto.Ed25519.signature sig) of
    (CryptoPassed pk', CryptoPassed sig') ->
      if Crypto.Ed25519.verify pk' content sig'
        then Right jwt
        else Left IncorrectSignature

    (CryptoFailed _, _) ->
      Left BadPublicKey

    (_, CryptoFailed _) ->
      Left BadSignature
    
  where
    Claims {iss = User.DID {publicKey = Key.Public pk}} = claims
    errOrPk = Crypto.Ed25519.publicKey . B64.Scrubbed.scrubB64 $ encodeUtf8 pk
    content = UTF8.stripPadding . Lazy.toStrict $ encode header <> "." <> encode claims
