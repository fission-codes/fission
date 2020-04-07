-- |

module Fission.Web.Auth.JWT.Validation
  ( parse
  , fromBearer
  , checkTime
  , checkSignature
  , checkEd25519Signature
  , checkRSA2048Signature
  ) where

import           Crypto.Error
import           Crypto.Hash.Algorithms (SHA256 (..))

import qualified Data.Binary as Binary

import qualified Codec.Crypto.RSA.Pure as Codec.RSA

import qualified Crypto.PubKey.Ed25519    as Crypto.Ed25519
import qualified Crypto.PubKey.RSA        as Crypto.RSA
import qualified Crypto.PubKey.RSA.PKCS15 as Crypto.RSA.PKCS

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text as Text

import           Fission.Prelude
import qualified Fission.Internal.Crypto as Crypto
import qualified Fission.Internal.UTF8   as UTF8
import           Fission.Key             as Key
import qualified Fission.User            as User

import           Fission.Web.Auth.JWT.Error as JWT
import           Fission.Web.Auth.JWT.Types as JWT

import qualified Fission.Web.Auth.Token.Bearer.Types as Auth.Bearer

parse :: MonadTime m => Auth.Bearer.Token -> m (Either JWT.Error JWT)
parse bearerToken = do
  now <- currentTime
  return (checkSignature =<< checkTime now =<< fromBearer bearerToken)

fromBearer :: Auth.Bearer.Token -> Either JWT.Error JWT
fromBearer (Auth.Bearer.Token bearerToken) =
  case eitherDecode (Lazy.fromStrict bearerToken) of
    Left  _   -> Left JWT.ParseError
    Right jwt -> Right jwt

checkTime :: UTCTime -> JWT -> Either JWT.Error JWT
checkTime now jwt@JWT {claims = JWT.Claims { exp, nbf }} = do
  case (now > exp, nbf) of
    (True, _) -> Left JWT.Expired
    (_, Just nbf') -> if now < nbf' then Left JWT.TooEarly else Right jwt
    _ -> Right jwt

checkSignature :: JWT -> Either JWT.Error JWT
checkSignature jwt@JWT {header = JWT.Header {alg}} =
  case alg of
    Ed25519 -> checkEd25519Signature jwt
    RSA2048 -> checkRSA2048Signature jwt
 
checkRSA2048Signature :: JWT -> Either JWT.Error JWT
checkRSA2048Signature jwt@JWT {..} =
  if Crypto.RSA.PKCS.verify (Just SHA256) pk content sig64
    then Right jwt
    else Left IncorrectSignature
 
  where
    pk      = Crypto.RSA.PublicKey {..}
    sig64   = encodeUtf8 $ textDisplay $ displayShow sig
    content = Lazy.toStrict $ encode header <> "." <> encode claims
 
    Claims {iss = User.DID {publicKey = Key.Public pk'}}  = claims
    Codec.RSA.PublicKey {public_size, public_n, public_e} =
      Binary.decode . Lazy.fromStrict $ encodeUtf8 pk'

checkEd25519Signature :: JWT -> Either JWT.Error JWT
checkEd25519Signature jwt@JWT {..} =
  case (Crypto.base64ToEdPubKey (encodeUtf8 pk), Crypto.Ed25519.signature sig) of
    (CryptoPassed pk', CryptoPassed sig') ->
      if Crypto.Ed25519.verify pk' content sig'
        then Right jwt
        else trace (Text.pack $ show content <> " / " <> show pk' <> " / " <> show sig') $ Left IncorrectSignature

    (CryptoFailed _, _) ->
      Left BadPublicKey

    (_, CryptoFailed _) ->
      Left BadSignature
    
  where
    Claims {iss = User.DID {publicKey = Key.Public pk}} = claims
   --  content   = Lazy.toStrict $ encode header <> "." <> encode claims
    content =
      UTF8.stripOptionalSuffixBS "=" $
      UTF8.stripOptionalSuffixBS "=" $
        {-Crypto.toBase64 $ -} Lazy.toStrict $ encode header <> "." <> encode claims
