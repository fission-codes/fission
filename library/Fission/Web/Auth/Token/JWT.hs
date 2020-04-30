module Fission.Web.Auth.Token.JWT
  ( JWT    (..)
  , Claims (..)
  , Proof  (..)

  , signEd25519
  , signRS256

  -- * reexports
 
  , module Fission.Web.Auth.Token.JWT.Header.Types
  ) where

import qualified System.IO.Unsafe as Unsafe

import           Crypto.Random          (MonadRandom (..))
import           Crypto.Hash.Algorithms (SHA256 (..))
 
import qualified Crypto.PubKey.RSA        as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA.PKCS15

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.PubKey.Ed25519 (toPublic)
 
import qualified Data.ByteString.Base64.URL as BS.B64.URL

import           Network.IPFS.CID.Types

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Fission.Prelude

import qualified Fission.Key.Asymmetric.Algorithm.Types as Algorithm

import qualified Fission.Internal.Base64.URL         as B64.URL
import qualified Fission.Internal.UTF8               as UTF8
import qualified Fission.Internal.RSA2048.Pair.Types as RSA2048

import           Fission.Key as Key

import           Fission.Authorization.Potency.Types
import           Fission.User.DID.Types

import           Fission.Web.Auth.Token.JWT.Signature             as Signature
import qualified Fission.Web.Auth.Token.JWT.Signature.RS256.Types as RS256
import           Fission.Web.Auth.Token.JWT.Header.Types (Header (..))




-- Orphans

import           Fission.Internal.Orphanage.Ed25519.SecretKey ()
import           Fission.Internal.Orphanage.CID               ()

-- | An RFC 7519 extended with support for Ed25519 keys,
--     and some specifics (claims, etc) for Fission's use case
data JWT = JWT
  { header :: !Header
  , claims :: !Claims
  , sig    :: !Signature.Signature
  } deriving (Show, Eq)

instance Arbitrary JWT where
  arbitrary = do
    header   <- arbitrary
    (pk, sk) <- case alg header of
      Algorithm.RSA2048 -> do
        RSA2048.Pair pk' sk' <- arbitrary
        return (RSAPublicKey pk', Left sk')

      Algorithm.Ed25519 -> do
        sk' <- arbitrary
        return (Ed25519PublicKey (toPublic sk'), Right sk')

    claims' <- arbitrary

    let
      claims = claims' {sender = DID pk Key }
   
      sig' = case sk of
        Left rsaSK -> Unsafe.unsafePerformIO $ signRS256 header claims rsaSK
        Right edSK -> Right $ signEd25519 header claims edSK

    case sig' of
      Left _    -> error "Unable to sign JWT"
      Right sig -> return JWT {..}
 
instance ToJSON JWT where
  toJSON JWT {..} = String $ content <> "." <> textDisplay sig
    where
      content = decodeUtf8Lenient $ encodeB64 header <> "." <> encodeB64 claims

      encodeB64 jsonable =
        jsonable
          |> encode
          |> Lazy.toStrict
          |> UTF8.stripQuotesBS
          |> BS.B64.URL.encode
          |> UTF8.stripPadding

instance FromJSON JWT where
  parseJSON = withText "JWT.Token" \txt ->
    case Text.split (== '.') txt of
      [rawHeader, rawClaims, rawSig] -> do
        header <- withEmbeddedJSON "Header" parseJSON $ jsonify rawHeader
        claims <- withEmbeddedJSON "Claims" parseJSON $ jsonify rawClaims
        sig    <- Signature.parse (alg header) (toJSON rawSig)
        return JWT {..}

      _ ->
        fail $ "Wrong number of JWT segments in:  " <> show txt
    where
      jsonify = toJSON . decodeUtf8Lenient . BS.B64.URL.decodeLenient . encodeUtf8

------------
-- Claims --
------------

data Claims = Claims
  -- Dramatis Personae
  { sender    :: !DID
  , receiver  :: !DID
  -- Authorization Scope
  , scope     :: !Text
  , potency   :: !Potency
  , proof     :: !Proof
  -- Temporal Bounds
  , exp       :: !UTCTime
  , nbf       :: !UTCTime
  } deriving Show

instance Display Claims where
  textDisplay = Text.pack . show

instance Eq Claims where
  jwtA == jwtB = eqWho && eqAuth && eqTime
    where
      eqWho = sender jwtA == sender   jwtB
         && receiver jwtA == receiver jwtB
 
      eqAuth = scope jwtA == scope   jwtB
          &&   proof jwtA == proof   jwtB
          && potency jwtA == potency jwtB

      eqTime = roundUTC (exp jwtA) == roundUTC (exp jwtB)
            && roundUTC (nbf jwtA) == roundUTC (nbf jwtB)

instance Arbitrary Claims where
  arbitrary = do
    sender  <- arbitrary
    scope'  <- arbitrary
    potency <- arbitrary
    proof   <- arbitrary
    exp     <- arbitrary
    nbf     <- arbitrary
    pk      <- arbitrary

    let
      scope = "/" <> scope'
      receiver = DID
        { publicKey = pk
        , method    = Key
        }

    return Claims {..}

instance ToJSON Claims where
  toJSON Claims {..} = object
    [ "iss" .= sender
    , "aud" .= receiver
    --
    , "prf" .= proof
    , "ptc" .= potency
    , "scp" .= scope
    --
    , "nbf" .= toSeconds nbf
    , "exp" .= toSeconds exp
    ]

instance FromJSON Claims where
  parseJSON = withObject "JWT.Payload" \obj -> do
    sender   <- obj .: "iss"
    receiver <- obj .: "aud"
    --
    scope   <- obj .:  "scp"
    potency <- obj .:? "ptc" .!= AuthNOnly
    proof   <- obj .:? "prf" .!= RootCredential
    --
    nbf <- fromSeconds <$> obj .: "nbf"
    exp <- fromSeconds <$> obj .: "exp"

    return Claims {..}

-----------
-- Proof --
-----------

data Proof
  = RootCredential
  | Nested    Text JWT
  | Reference CID
  deriving (Show, Eq)

instance Arbitrary Proof where
  arbitrary =
    [ (1, Nested <$> arbitrary <*> arbitrary)
    , (3, pure RootCredential)
    ] |> frequency
      |> fmap \case
        Nested _ jwt -> Nested (Text.dropEnd 1 . Text.drop 1 . decodeUtf8Lenient . Lazy.toStrict $ encode jwt) jwt
        prf          -> prf

instance ToJSON Proof where
  toJSON = \case
    Reference cid   -> toJSON cid
    Nested    raw _ -> String raw
    RootCredential  -> Null

instance FromJSON Proof where
  parseJSON Null = return RootCredential
  parseJSON val  = withText "Credential Proof" resolver val
    where
      resolver txt =
        case Text.stripPrefix "eyJ" txt of -- i.e. starts with Base64 encoded '{'
          Just _  -> Nested txt <$> parseJSON val
          Nothing -> Reference <$> parseJSON val

-----------------------
-- Signature Helpers --
-----------------------

signEd25519 :: Header -> Claims -> Ed25519.SecretKey -> Signature.Signature
signEd25519 header claims sk =
  Signature.Ed25519 . Key.signWith sk . encodeUtf8 $ B64.URL.encodeJWT header claims

signRS256 ::
  MonadRandom m
  => Header
  -> Claims
  -> RSA.PrivateKey
  -> m (Either RSA.Error Signature.Signature)
signRS256 header claims sk =
  RSA.PKCS15.signSafer (Just SHA256) sk (encodeUtf8 $ B64.URL.encodeJWT header claims) <&> \case
    Left err  -> Left err
    Right sig -> Right . Signature.RS256 $ RS256.Signature sig
