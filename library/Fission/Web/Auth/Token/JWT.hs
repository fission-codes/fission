module Fission.Web.Auth.Token.JWT
  ( JWT    (..)
  , Claims (..)
  , Proof  (..)

  , signEd25519
  , signRS256

  -- * reexports

  , module Fission.Web.Auth.Token.JWT.Header.Types
  , module Fission.Web.Auth.Token.JWT.RawContent
  ) where

import qualified System.IO.Unsafe                                 as Unsafe

import           Crypto.Hash.Algorithms                           (SHA256 (..))
import           Crypto.Random                                    (MonadRandom (..))

import qualified Crypto.PubKey.RSA                                as RSA
import qualified Crypto.PubKey.RSA.PKCS15                         as RSA.PKCS15

import           Crypto.PubKey.Ed25519                            (toPublic)
import qualified Crypto.PubKey.Ed25519                            as Ed25519

import qualified Data.ByteString.Base64.URL                       as BS.B64.URL

import           Network.IPFS.CID.Types

import qualified RIO.ByteString.Lazy                              as Lazy
import qualified RIO.Text                                         as Text

import qualified Fission.Internal.Base64.URL                      as B64.URL
import           Fission.Prelude

import qualified Fission.Key.Asymmetric.Algorithm.Types           as Algorithm

import qualified Fission.Internal.RSA2048.Pair.Types              as RSA2048
import qualified Fission.Internal.UTF8                            as UTF8

import           Fission.Key                                      as Key

import           Fission.Authorization.Potency.Types
import           Fission.User.DID.Types

import           Fission.Web.Auth.Token.JWT.Header.Types          (Header (..))
import           Fission.Web.Auth.Token.JWT.Signature             as Signature
import qualified Fission.Web.Auth.Token.JWT.Signature.RS256.Types as RS256
import qualified Fission.Web.Auth.Token.JWT.RawContent            as JWT

import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

-- Reexports

import           Fission.Web.Auth.Token.JWT.RawContent

-- Orphans

import           Fission.Internal.Orphanage.CID ()
import           Fission.Internal.Orphanage.Ed25519.SecretKey ()

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
      claims = claims' {sender = DID Key pk}
   
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
        fail $ "Wrong number of JWT segments in:  " <> Text.unpack txt
    where
      jsonify = toJSON . decodeUtf8Lenient . BS.B64.URL.decodeLenient . encodeUtf8

------------
-- Claims --
------------

data Claims = Claims
  -- Dramatis Personae
  { sender   :: !DID
  , receiver :: !DID
  -- Authorization Target
  , resource :: !(Scope Resource)
  , potency  :: !Potency
  , proof    :: !Proof
  -- Temporal Bounds
  , exp      :: !UTCTime
  , nbf      :: !UTCTime
  } deriving Show

instance Display Claims where
  textDisplay = Text.pack . show

instance Eq Claims where
  jwtA == jwtB = eqWho && eqAuth && eqTime
    where
      eqWho = sender jwtA == sender   jwtB
         && receiver jwtA == receiver jwtB

      eqAuth = resource jwtA == resource jwtB
             &&   proof jwtA == proof    jwtB
             && potency jwtA == potency  jwtB

      eqTime = roundUTC (exp jwtA) == roundUTC (exp jwtB)
            && roundUTC (nbf jwtA) == roundUTC (nbf jwtB)

instance Arbitrary Claims where
  arbitrary = do
    sender   <- arbitrary
    resource <- arbitrary
    potency  <- arbitrary
    proof    <- arbitrary
    exp      <- arbitrary
    nbf      <- arbitrary
    pk       <- arbitrary

    let
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
    , "rsc" .= resource
    --
    , "nbf" .= toSeconds nbf
    , "exp" .= toSeconds exp
    ]

instance FromJSON Claims where
  parseJSON = withObject "JWT.Payload" \obj -> do
    sender   <- obj .: "iss"
    receiver <- obj .: "aud"
    --
    resource <- obj .:  "rsc"
    potency  <- obj .:? "ptc" .!= AuthNOnly
    proof    <- obj .:? "prf" .!= RootCredential
    --
    nbf <- fromSeconds <$> obj .: "nbf"
    exp <- fromSeconds <$> obj .: "exp"

    return Claims {..}

-----------
-- Proof --
-----------

data Proof
  = RootCredential
  | Nested    JWT.RawContent JWT
  | Reference CID
  deriving (Show, Eq)

instance Arbitrary Proof where
  arbitrary = frequency
    [ (1, nested)
    , (5, pure RootCredential)
    ]
    where
      nested = do
        innerJWT@(JWT {..}) <- arbitrary
        let rawContent = RawContent $ B64.URL.encodeJWT header claims
        return $ Nested rawContent innerJWT

instance ToJSON Proof where
  toJSON = \case
    RootCredential ->
      Null

    Reference cid ->
      toJSON cid

    Nested (JWT.RawContent raw) JWT {sig} ->
      String (raw <> "." <> textDisplay sig)

instance FromJSON Proof where
  parseJSON Null = return RootCredential
  parseJSON val  = withText "Credential Proof" resolver val
    where
      resolver txt =
        if "eyJ" `Text.isPrefixOf` txt -- i.e. starts with Base64 encoded '{'
          then Nested (JWT.contentOf txt) <$> parseJSON val
          else Reference <$> parseJSON val

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
