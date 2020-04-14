module Fission.Web.Auth.JWT.Types -- FIXME not just types
  ( JWT    (..)
  , Claims (..)
  , Proof  (..)

  , signEd25519
  , signRS256
  , parseSig

  -- * reexports
 
  , module Fission.Web.Auth.JWT.Header.Types
  ) where

import           Crypto.Random          (MonadRandom (..))
import qualified System.IO.Unsafe as Unsafe

import Fission.Key.Asymmetric.Algorithm.Types as Algorithm

import           Crypto.Hash.Algorithms (SHA256 (..))
import Network.IPFS.CID.Types

import Fission.Authorization.Potency.Types

import qualified Crypto.PubKey.RSA        as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA.PKCS15

import           Fission.Prelude
import qualified Fission.Internal.Base64.URL as B64.URL

import qualified Fission.Web.Auth.JWT.Signature.RS256.Types as RS256

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Crypto.PubKey.Ed25519 (toPublic)

import qualified Data.ByteString.Base64.URL as BS.B64.URL
import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import qualified Fission.Internal.UTF8 as UTF8
 
import qualified Fission.Key                            as Key
import qualified Fission.Key.Asymmetric.Algorithm.Types as Alg
import           Fission.User.DID.Types

import           Fission.Web.Auth.JWT.Header.Types (Header (..))

import           Fission.Web.Auth.JWT.Signature         as Signature
import           Fission.Web.Auth.JWT.Signature         as Sig

import qualified Fission.Internal.Base64 as B64

import           Fission.Internal.Orphanage.Ed25519.SecretKey ()
import           Fission.Internal.Orphanage.CID ()
import           Fission.Internal.RSA2048.Pair.Types

-- | An RFC 7519 extended with support for Ed25519 keys,
--     and some specifics (claims, etc) for Fission's use case
data JWT = JWT
  { header :: !Header
  , claims :: !Claims
  , sig    :: !Sig.Signature
  } deriving (Show, Eq)

instance Arbitrary JWT where
  arbitrary = do
    header  <- arbitrary
    claims' <- arbitrary

    case alg header of
      Alg.Ed25519 -> do
        sk <- arbitrary

        let
          did = DID
            { publicKey = Key.Public . B64.toB64ByteString $ toPublic sk
            , algorithm = Alg.Ed25519
            , method    = Key
            }

          claims = claims' { sender = did }
          sig    = signEd25519 header claims sk
         
        return JWT {..}

      Alg.RSA2048 ->
        genRSA header claims'
  
genRSA :: Header -> Claims -> Gen JWT
genRSA header claims' = do
  Pair _pk sk <- arbitrary

  let
    pk = Key.Public "FAKE_publickey"

    did = DID
      { publicKey = pk
      , algorithm = Alg.RSA2048
      , method    = Key
      }

    claims = claims' { sender = did }

  case Unsafe.unsafePerformIO $ signRS256 header claims sk of
    Right sig -> return JWT {..}
    Left  _   -> genRSA header claims

instance ToJSON JWT where
  toJSON JWT {..} = String . decodeUtf8Lenient $
    encodeB64 header <> "." <> encodeB64 claims <> "." <> signed
    where
      signed :: ByteString
      signed =
        case sig of
          Sig.Ed25519 edSig                 -> encodeSig edSig
          Sig.RS256 (RS256.Signature rsSig) -> encodeSig rsSig

      encodeSig raw =
        raw
          |> B64.toB64ByteString
          |> decodeUtf8Lenient
          |> B64.URL.encode
          |> encodeUtf8
          |> UTF8.stripPadding

      encodeB64 jsonable =
        jsonable
          |> encode
          |> Lazy.toStrict
          |> UTF8.stripQuotesBS
          |> BS.B64.URL.encode
          |> UTF8.stripPadding

instance FromJSON JWT where
  parseJSON = withText "JWT.Token" \txt ->
    txt
      |> encodeUtf8
      |> B64.toByteString
      |> UTF8.stripPadding
      |> Lazy.fromStrict
      |> Char8.split '.'
      |> \case
          [rawHeader, rawClaims, rawSig] -> do
            let
              result = do
                header <- B64.URL.addPadding rawHeader
                claims <- B64.URL.addPadding rawClaims
                sig    <- Signature.parse (alg header) $  "\"" <> rawSig <> "\""
                return JWT {..}

            case result of
              Left  err   -> fail err
              Right token -> return token

          _ ->
            fail $ show txt <> " is not a valid JWT.Token"

-------------------------------------------------------------
 
-- FIXME NOTE: should add PKs to DNS as well

data Proof
  = RootCredential
  | Nested    ByteString -- FIXME
  | Reference CID
  deriving (Show, Eq)

instance Arbitrary Proof where
  arbitrary = oneof
    [ Reference <$> arbitrary
    , Nested    <$> arbitrary
    , pure RootCredential
    ]

instance ToJSON Proof where
  toJSON = \case
    Reference cid  -> toJSON cid
    Nested    raw  -> String $ decodeUtf8Lenient raw
    RootCredential -> Null

instance FromJSON Proof where
  parseJSON Null = return RootCredential
  parseJSON val  = withText "Credential Proof" resolver val
    where
      resolver txt =
        case Text.stripPrefix "eyJ" txt of -- i.e. starts with Base64 encoded '{'
          Just _  -> Reference <$> parseJSON (String txt)
          Nothing -> pure . Nested $ encodeUtf8 txt

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
  , nbf       :: !(Maybe UTCTime)
  } deriving Show

instance Display Claims where
  textDisplay = Text.pack . show

instance Eq Claims where
  jwtA == jwtB = eqWho && eqAuth && eqTime
    where
      eqWho = (sender jwtA == sender   jwtB)
         && (receiver jwtA == receiver jwtB)
 
      eqAuth = (scope jwtA == scope   jwtB)
          &&   (proof jwtA == proof   jwtB)
          && (potency jwtA == potency jwtB)

      eqTime = (roundUTC    (exp jwtA) ==  roundUTC    (exp jwtB))
            && (roundUTC <$> nbf jwtA) == (roundUTC <$> nbf jwtB)

instance Arbitrary Claims where
  arbitrary = do
    sender   <- arbitrary
    receiver <- arbitrary
    --
    scope   <- arbitrary
    potency <- arbitrary
    proof   <- arbitrary
    --
    exp <- fromSeconds . toSeconds <$> arbitrary
    nbf <- arbitrary

    return Claims {..}

instance ToJSON Claims where
  toJSON Claims {..} = object
    [ "iss" .= sender
    , "aud" .= receiver
    --
    , "prf" .= proof
    , "pot" .= potency
    , "scp" .= scope -- FIXME change to `pwd`!
    --
    , "nbf" .= fmap toSeconds nbf
    , "exp" .= toSeconds exp
    ]

instance FromJSON Claims where
  parseJSON = withObject "JWT.Payload" \obj -> do
    sender   <- obj .: "iss"
    receiver <- obj .: "aud"
    --
    scope   <- obj .:  "scp"
    potency <- obj .:? "pot" .!= AuthNOnly
    proof   <- obj .:  "prf"
    --
    nbf <- fmap fromSeconds <$> obj .: "nbf"
    exp <-      fromSeconds <$> obj .: "exp"

    return Claims {..}

---------------------------------------------------------

signEd25519 :: Header -> Claims -> Ed25519.SecretKey -> Sig.Signature
signEd25519 header claims sk =
  Sig.Ed25519 . Key.signWith sk $ B64.URL.encodeJWT header claims

signRS256 ::
  MonadRandom m
  => Header
  -> Claims
  -> RSA.PrivateKey
  -> m (Either RSA.Error Sig.Signature)
signRS256 header claims sk =
  RSA.PKCS15.signSafer (Just SHA256) sk (B64.URL.encodeJWT header claims) <&> \case
    Left err  -> Left err
    Right sig -> Right . Sig.RS256 $ RS256.Signature sig

parseSig :: Algorithm -> Lazy.ByteString -> Either String Sig.Signature
parseSig alg lazyBS =
  case alg of
    Algorithm.RSA2048 -> Sig.RS256   <$> eitherDecode lazyBS
    Algorithm.Ed25519 -> Sig.Ed25519 <$> eitherDecode lazyBS
