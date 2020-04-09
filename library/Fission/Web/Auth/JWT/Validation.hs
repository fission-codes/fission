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

import qualified RIO.ByteString.Lazy as Lazy
-- import qualified RIO.Text as Text

import           Fission.Prelude
import qualified Fission.Internal.UTF8            as UTF8
import qualified Fission.Internal.Base64.Scrubbed as B64.Scrubbed

import           Fission.Key  as Key
import qualified Fission.User as User

import           Fission.Web.Auth.JWT.Error as JWT
import           Fission.Web.Auth.JWT.Types as JWT

import           Fission.Web.Auth.JWT.Signature.Types       as Signature
import qualified Fission.Web.Auth.JWT.Signature.RS256.Types as RS256

import qualified Fission.Internal.Crypto as Crypto
import qualified Fission.Internal.Base64 as B64
import qualified Fission.Internal.Base64.URL as B64.URL

check :: MonadTime m => JWT -> m (Either JWT.Error JWT)
check jwt = check' jwt <$> currentTime

check' :: JWT -> UTCTime -> Either JWT.Error JWT
check' jwt now =
  pure jwt
--     >>= checkTime now
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
checkRSA2048Signature jwt@JWT {..} (RS256.Signature innerSig) = do
  case Crypto.decodeToRSA2048PK pk' of
    Left _ ->
      Left BadPublicKey

    Right pk ->
      if Crypto.RSA.PKCS.verify (Nothing :: Maybe SHA256) pk content innerSig
      -- if Crypto.RSA.PKCS.verify (Just SHA256) pk content innerSig
        then Right jwt
        else Left IncorrectSignature
 
  where
    Claims {iss = User.DID {publicKey = Key.Public pk'}} = claims
    -- content = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1ODY0MjAwOTAsImlzcyI6ImRpZDprZXk6ejEzVjNTb2cyWWFVS2hkR0NtZ3g5VVp1VzFvMVNoRkpZYzZEdkdZZTdOVHQ2ODlOb0wzZkF2TlhDQkhNbnVab2JVVm9yVGtQZ1FBbVl3TmdQS1pGZVp0Z0Z0OE1nUWNweGFXVFV4NjVocUtmcHVSd3JqZnFHSlRMbmk1dnoxZzg1RWlDdUhLeW9XMmp1cW54VXRIOUZkV0JVR0VwVWlvTk1CNFZydk1GWlE4VHpSQmJ1SlZyS3pFMURhVWdvYTdqcnM5bU1Ubmh1Y1h4NW1uOWE0aVlEU0xTZHI2bzVuckQ4c0VEbllmdWNYc0w4dHVNQjlhaUhjYWlidE1Ya3oxUmlXRXNndzhTOTliWjgxQmNuM3l2ZTJHOUtrU3F0WHNlNmZTaXJXaTFyS2hUMUxSQ0VCSnhYbkFxY25NZlQ0cU5ORE5USlUzb2VWUmVWYzJ6bXhhZUM4aGNIWXAzRjdnNWtjclJONEdvc2lQdnpWVkdhNXZDTnBrNHc4OHJkTVZiek5uanJVWlNhRmY3RVRmWmhRd25MQ0ciLCJqdGkiOiI1ZjBiN2RhZi1lOTZkLTQyZWItYmE1ZC0yZTVlNjExZTQ3ZWMiLCJpYXQiOjE1ODY0MTYyNzB9"
  --   content = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1ODY0MjA0MDgsImlzcyI6ImRpZDprZXk6ejEzVjNTb2cyWWFVS2hkR0NtZ3g5VVp1VzFvMVNoRkpZYzZEdkdZZTdOVHQ2ODlOb0wzZkF2TlhDQkhNbnVab2JVVm9yVGtQZ1FBbVl3TmdQS1pGZVp0Z0Z0OE1nUWNweGFXVFV4NjVocUtmcHVSd3JqZnFHSlRMbmk1dnoxZzg1RWlDdUhLeW9XMmp1cW54VXRIOUZkV0JVR0VwVWlvTk1CNFZydk1GWlE4VHpSQmJ1SlZyS3pFMURhVWdvYTdqcnM5bU1Ubmh1Y1h4NW1uOWE0aVlEU0xTZHI2bzVuckQ4c0VEbllmdWNYc0w4dHVNQjlhaUhjYWlidE1Ya3oxUmlXRXNndzhTOTliWjgxQmNuM3l2ZTJHOUtrU3F0WHNlNmZTaXJXaTFyS2hUMUxSQ0VCSnhYbkFxY25NZlQ0cU5ORE5USlUzb2VWUmVWYzJ6bXhhZUM4aGNIWXAzRjdnNWtjclJONEdvc2lQdnpWVkdhNXZDTnBrNHc4OHJkTVZiek5uanJVWlNhRmY3RVRmWmhRd25MQ0ciLCJqdGkiOiJkZGQ3MjQ5OS0xZmZmLTRhOTMtYTUxMy00YjBiYjQwNTYwNGIiLCJpYXQiOjE1ODY0MTY2MDZ9"
    content = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE1ODYzNzEzNDkxMjYsImlzcyI6ImRpZDprZXk6ejEzVjNTb2cyWWFVS2hkR0NtZ3g5VVp1VzFvMVNoRkpZYzZEdkdZZTdOVHQ2ODlOb0wzZkF2TlhDQkhNbnVab2JVVm9yVGtQZ1FBbVl3TmdQS1pGZVp0Z0Z0OE1nUWNweGFXVFV4NjVocUtmcHVSd3JqZnFHSlRMbmk1dnoxZzg1RWlDdUhLeW9XMmp1cW54VXRIOUZkV0JVR0VwVWlvTk1CNFZydk1GWlE4VHpSQmJ1SlZyS3pFMURhVWdvYTdqcnM5bU1Ubmh1Y1h4NW1uOWE0aVlEU0xTZHI2bzVuckQ4c0VEbllmdWNYc0w4dHVNQjlhaUhjYWlidE1Ya3oxUmlXRXNndzhTOTliWjgxQmNuM3l2ZTJHOUtrU3F0WHNlNmZTaXJXaTFyS2hUMUxSQ0VCSnhYbkFxY25NZlQ0cU5ORE5USlUzb2VWUmVWYzJ6bXhhZUM4aGNIWXAzRjdnNWtjclJONEdvc2lQdnpWVkdhNXZDTnBrNHc4OHJkTVZiek5uanJVWlNhRmY3RVRmWmhRd25MQ0ciLCJuYmYiOm51bGx9" -- "kHGfUlB-pog86ON5loYsLGPydcRvxk49ez-AI17ko6Bj8OBdHqnSwggHl3NMy9DCEg4N0Xkmp2X_-E4gnlJ9hCNEDJB2v61sbQCvqVJZtrF2RGAk8ZyCQLA5dHuRTvb8jhI453C-_ocnkH0Q-42Z8jI_JkeghauRqJanudBJQOcX2MbRrt84zYZ7eATRM-sXUdy2NtaGPwP2i5h2iNOZGlSnj-GsnFkzIBSPHBw2Wm1p-1MJ2DM_smWx_b_NfE379KsvDIRPZ0y_3ZPO3tCWAi9t69jls1_RKD5EdwkKFTnGbCWaHLdF-78-YvTJcQ2fsj4e5xD-Ok6shQvrkkLt5A"
    -- content = "kHGfUlB-pog86ON5loYsLGPydcRvxk49ez-AI17ko6Bj8OBdHqnSwggHl3NMy9DCEg4N0Xkmp2X_-E4gnlJ9hCNEDJB2v61sbQCvqVJZtrF2RGAk8ZyCQLA5dHuRTvb8jhI453C-_ocnkH0Q-42Z8jI_JkeghauRqJanudBJQOcX2MbRrt84zYZ7eATRM-sXUdy2NtaGPwP2i5h2iNOZGlSnj-GsnFkzIBSPHBw2Wm1p-1MJ2DM_smWx_b_NfE379KsvDIRPZ0y_3ZPO3tCWAi9t69jls1_RKD5EdwkKFTnGbCWaHLdF-78-YvTJcQ2fsj4e5xD-Ok6shQvrkkLt5A"
      -- encodeUtf8 . B64.URL.encode $ decodeUtf8Lenient $
      --   UTF8.stripPadding (Lazy.toStrict (encode header)) <> "." <> UTF8.stripPadding (Lazy.toStrict (encode claims))
    -- content =
    --   (B64.toB64ByteString $ Lazy.toStrict (encode header)) <> "."
    --     <> (B64.toB64ByteString $ Lazy.toStrict $ encode claims)
    --   -- (encodeUtf8 $ B64.URL.encode $ decodeUtf8Lenient $ B64.toB64ByteString $ Lazy.toStrict (encode header)) <> "."
    --   --   <> (encodeUtf8 $ B64.URL.encode $ decodeUtf8Lenient $ B64.toB64ByteString $ Lazy.toStrict $ encode claims)

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
    errOrPk = Crypto.Ed25519.publicKey $ B64.Scrubbed.scrubB64 pk
    content = UTF8.stripPadding . Lazy.toStrict $ encode header <> "." <> encode claims
