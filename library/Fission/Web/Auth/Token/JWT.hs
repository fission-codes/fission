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
import qualified Data.ByteString.Lazy.Char8 as Char8
 
import           Network.IPFS.CID.Types

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Fission.Prelude

import qualified Fission.Internal.Base64     as B64
import qualified Fission.Internal.Base64.URL as B64.URL
import qualified Fission.Internal.UTF8       as UTF8
 
import           Fission.Internal.RSA2048.Pair.Types

import qualified Fission.Key                            as Key
import           Fission.Key.Asymmetric.Algorithm.Types as Algorithm

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
    header  <- arbitrary
    claims' <- arbitrary

    case alg header of
      Algorithm.Ed25519 -> do
        sk <- arbitrary

        let
          did = DID
            { publicKey = Key.Public . B64.toB64ByteString $ toPublic sk
            , algorithm = Algorithm.Ed25519
            , method    = Key
            }

          claims = claims' { sender = did }
          sig    = signEd25519 header claims sk
         
        return JWT {..}

      Algorithm.RSA2048 ->
        genRSA header claims'
  
genRSA :: Header -> Claims -> Gen JWT
genRSA header claims' = do
  Pair _pk sk <- arbitrary

  let
    pk = Key.Public "FAKE_publickey"

    did = DID
      { publicKey = pk
      , algorithm = Algorithm.RSA2048
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
          Signature.Ed25519 edSig                 -> encodeSig edSig
          Signature.RS256 (RS256.Signature rsSig) -> encodeSig rsSig

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
          [rawHeader, rawClaims, rawSig] ->
            either fail pure do
              header <- B64.URL.addPadding rawHeader
              claims <- B64.URL.addPadding rawClaims
              sig    <- Signature.parse (alg header) $  "\"" <> rawSig <> "\""
              return JWT {..}

          _ ->
            fail $ show txt <> " is not a valid JWT.Token"

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
    sender <- arbitrary
   
    let receiver = DID
          { publicKey = Key.Public "AAAAC3NzaC1lZDI1NTE5AAAAIB7/gFUQ9llI1BTrEjW7Jq6fX6JLsK1J4wXK/dn9JMcO"
          , algorithm = Algorithm.Ed25519
          , method    = Key
          }

    scope   <- arbitrary
    potency <- arbitrary
    proof   <- arbitrary

    exp <- fromSeconds . toSeconds <$> arbitrary
    nbf <- arbitrary

    return Claims {..}

instance ToJSON Claims where
  toJSON Claims {..} = object
    [ "iss" .= sender
    , "aud" .= receiver
    --
    , "prf" .= proof
    , "pcy" .= potency
    , "scp" .= scope
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
    potency <- obj .:? "pcy" .!= AuthNOnly
    proof   <- obj .:? "prf" .!= RootCredential
    --
    nbf <- fmap fromSeconds <$> obj .:? "nbf"
    exp <-      fromSeconds <$> obj .:  "exp"

    return Claims {..}

-----------
-- Proof --
-----------

data Proof
  = RootCredential
  | Nested    ByteString JWT
  | Reference CID
  deriving (Show, Eq)

instance Arbitrary Proof where
  arbitrary =
    [ (1, Nested <$> arbitrary <*> arbitrary)
    , (4, pure RootCredential)
    ] |> frequency
      |> fmap \case
        Nested _ jwt -> Nested (Lazy.toStrict $ encode jwt) jwt
        prf          -> prf

instance ToJSON Proof where
  toJSON = \case
    Reference cid   -> toJSON cid
    Nested    raw _ -> String $ decodeUtf8Lenient raw
    RootCredential  -> Null

instance FromJSON Proof where
  parseJSON Null = return RootCredential
  parseJSON val  = withText "Credential Proof" resolver val
    where
      resolver txt =
        case Text.stripPrefix "eyJ" txt of -- i.e. starts with Base64 encoded '{'
          Just _  -> Reference <$> parseJSON (String txt)
          Nothing -> Nested (encodeUtf8 txt) <$> parseJSON (String txt)

-----------------------
-- Signature Helpers --
-----------------------

signEd25519 :: Header -> Claims -> Ed25519.SecretKey -> Signature.Signature
signEd25519 header claims sk =
  Signature.Ed25519 . Key.signWith sk $ B64.URL.encodeJWT header claims

signRS256 ::
  MonadRandom m
  => Header
  -> Claims
  -> RSA.PrivateKey
  -> m (Either RSA.Error Signature.Signature)
signRS256 header claims sk =
  RSA.PKCS15.signSafer (Just SHA256) sk (B64.URL.encodeJWT header claims) <&> \case
    Left err  -> Left err
    Right sig -> Right . Signature.RS256 $ RS256.Signature sig
