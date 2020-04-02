-- |

module Fission.Web.Auth.JWT.Validation
  ( parse
  , pureParse
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

import           Fission.Prelude
import qualified Fission.Internal.Crypto as Crypto
import           Fission.Key             as Key
import qualified Fission.User            as User

import           Fission.Web.Auth.JWT.Error as JWT
import           Fission.Web.Auth.JWT.Types as JWT

import qualified Fission.Web.Auth.Token.Bearer.Types as Auth.Bearer

parse :: MonadTime m => Auth.Bearer.Token -> m (Either JWT.Error JWT) -- NOTE: was validate
parse bearerToken = do
  now <- currentTime
  return (checkSignature =<< checkTime now =<< pureParse bearerToken)

pureParse :: Auth.Bearer.Token -> Either JWT.Error JWT
pureParse (Auth.Bearer.Token bearerToken) =
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
    RSA2048 -> undefined -- FIXME!!!!!!!
 
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
  case (cryptoPK, cryptoSig) of
    (CryptoPassed pk', CryptoPassed sig') ->
      if Crypto.Ed25519.verify pk' content sig'
        then Right jwt
        else Left IncorrectSignature

    _ ->
      Left BadSignature
  where
    Claims {iss = User.DID {publicKey = Key.Public pk}} = claims
    cryptoPK  = Crypto.base64ToEdPubKey $ encodeUtf8 pk
    cryptoSig = Crypto.base64ToSignature sig64
    sig64     = encodeUtf8 $ textDisplay $ displayShow sig
    content   = Crypto.pack . Lazy.toStrict $ encode header <> "." <> encode claims
