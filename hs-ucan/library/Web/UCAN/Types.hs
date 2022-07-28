module Web.UCAN.Types
  ( UCAN   (..)
  , Claims (..)
  , Proof  (..)

  , signEd25519
  , signRS256

  -- * reexports

  , module Web.UCAN.Header.Types
  , module Web.UCAN.RawContent
  ) where

import qualified System.IO.Unsafe                              as Unsafe

import           Crypto.Hash.Algorithms                        (SHA256 (..))
import           Crypto.Random                                 (MonadRandom (..))

import qualified Crypto.PubKey.RSA                             as RSA
import qualified Crypto.PubKey.RSA.PKCS15                      as RSA.PKCS15

import           Crypto.PubKey.Ed25519                         (toPublic)
import qualified Crypto.PubKey.Ed25519                         as Ed25519

import qualified Crypto.Secp256k1                              as Secp256k1

import           Data.Aeson
import qualified Data.ByteString.Base64.URL                    as BS.B64.URL

import           Network.IPFS.CID.Types

import qualified RIO.ByteString.Lazy                           as Lazy
import qualified RIO.Text                                      as Text

import qualified Servant.API                                   as Servant

import           RIO                                           hiding (exp)
import           RIO.Time

import           Test.QuickCheck


import           Crypto.Key.Asymmetric                         as Key
import qualified Crypto.Key.Asymmetric.Algorithm.Types         as Algorithm

import           Web.DID.Types                                 as DID

import           Web.UCAN.Header.Types                         (Header (..))
import qualified Web.UCAN.RawContent                           as UCAN
import           Web.UCAN.Signature                            as Signature
import qualified Web.UCAN.Signature.RS256.Types                as RS256
import qualified Web.UCAN.Signature.Secp256k1.Types            as Secp256k1

import qualified Web.UCAN.Internal.Base64.URL                  as B64.URL
import           Web.UCAN.Internal.Orphanage.Ed25519.SecretKey ()
import qualified Web.UCAN.Internal.RSA2048.Pair.Types          as RSA2048
import           Web.UCAN.Internal.Time
import qualified Web.UCAN.Internal.UTF8                        as UTF8


-- Reexports

import           Web.UCAN.RawContent

-- | An RFC 7519 extended with support for Ed25519 keys,
--     and some specifics (claims, etc) for Fission's use case
data UCAN fct rsc ptc = UCAN
  { header :: Header
  , claims :: Claims fct rsc ptc
  , sig    :: Signature.Signature
  } deriving (Show, Eq)

instance (Show fct, Show rsc, Show ptc) => Display (UCAN fct rsc ptc) where
  textDisplay = Text.pack . show

instance
  ( Arbitrary fct
  , Arbitrary rsc
  , Arbitrary ptc
  , ToJSON fct
  , ToJSON rsc
  , ToJSON ptc
  ) => Arbitrary (UCAN fct rsc ptc) where
  arbitrary = do
    (header :: Header) <- arbitrary
    (claims' :: Claims fct rsc ptc) <- arbitrary

    (claims, sig') <- case alg header of
      Algorithm.RSA2048 -> do
        RSA2048.Pair pk sk <- arbitrary

        let claims = claims' {sender = DID.Key (RSAPublicKey pk)}

        case Unsafe.unsafePerformIO (signRS256 header claims sk) of
          Left _  -> return (claims, Nothing)
          Right s -> return (claims, Just s)

      Algorithm.Ed25519 -> do
        sk <- arbitrary

        let pk = toPublic sk
        let claims = claims' {sender = DID.Key (Ed25519PublicKey pk)}

        return (claims, Just $ signEd25519 header claims sk)

      Algorithm.Secp256k1 -> do
        sk <- arbitrary

        let pk = Secp256k1.derivePubKey sk
        let claims = claims' {sender = DID.Key (Secp256k1PublicKey pk)}

        return (claims, signSecp256k1 header claims sk)

    case sig' of
      Nothing  -> error "Unable to sign UCAN"
      Just sig -> return UCAN {..}

instance (ToJSON fct, ToJSON rsc, ToJSON ptc) => ToJSON (UCAN fct rsc ptc) where
  toJSON UCAN {..} = String $ content <> "." <> textDisplay sig
    where
      content = decodeUtf8Lenient $ encodeB64 header <> "." <> encodeB64 claims

      encodeB64 jsonable =
        jsonable
          & encode
          & Lazy.toStrict
          & UTF8.stripQuotesBS
          & BS.B64.URL.encode
          & UTF8.stripPadding

instance (FromJSON fct, FromJSON rsc, FromJSON ptc) => FromJSON (UCAN fct rsc ptc) where
  parseJSON = withText "UCAN.Token" \txt ->
    case Text.split (== '.') txt of
      [rawHeader, rawClaims, rawSig] -> do
        header <- withEmbeddedJSON "Header" parseJSON $ jsonify rawHeader
        claims <- withEmbeddedJSON "Claims" parseJSON $ jsonify rawClaims
        sig    <- Signature.parse (alg header) (toJSON rawSig)
        return UCAN {..}

      _ ->
        fail $ "Wrong number of JWT segments in:  " <> Text.unpack txt
    where
      jsonify = toJSON . decodeUtf8Lenient . BS.B64.URL.decodeLenient . encodeUtf8

instance
  ( ToJSON fct
  , ToJSON rsc
  , ToJSON ptc
  ) => Servant.ToHttpApiData (UCAN fct rsc ptc) where
  toUrlPiece ucan =
    ucan
      & encode
      & Lazy.toStrict
      & decodeUtf8Lenient
      & UTF8.stripQuotes

------------
-- Claims --
------------

data Claims fct rsc ptc = Claims
  -- Dramatis Personae
  { sender   :: DID
  , receiver :: DID
  -- Authorization Target
  , resource :: Maybe rsc
  , potency  :: Maybe ptc
  , proof    :: Proof fct rsc ptc
  -- 0.3.1
  , facts    :: [fct]
  -- Temporal Bounds
  , exp      :: UTCTime
  , nbf      :: UTCTime
  } deriving Show

instance (Show fct, Show rsc, Show ptc) => Display (Claims fct rsc ptc) where
  textDisplay = Text.pack . show

instance (Eq fct, Eq rsc, Eq ptc) => Eq (Claims fct rsc ptc) where
  jwtA == jwtB = eqWho && eqAuth && eqTime && eqFacts
    where
      eqWho = sender jwtA == sender   jwtB
         && receiver jwtA == receiver jwtB

      eqAuth = resource jwtA == resource jwtB
             &&   proof jwtA == proof    jwtB
             && potency jwtA == potency  jwtB

      eqTime = roundUTC (exp jwtA) == roundUTC (exp jwtB)
            && roundUTC (nbf jwtA) == roundUTC (nbf jwtB)

      eqFacts = facts jwtA == facts jwtB

instance
  ( Arbitrary fct
  , Arbitrary rsc
  , Arbitrary ptc
  , ToJSON fct
  , ToJSON rsc
  , ToJSON ptc
  ) => Arbitrary (Claims fct rsc ptc) where
  arbitrary = do
    sender   <- arbitrary
    resource <- arbitrary
    potency  <- arbitrary
    proof    <- arbitrary
    facts    <- arbitrary
    exp      <- arbitrary
    nbf      <- arbitrary
    pk       <- arbitrary

    let receiver = DID.Key pk

    return Claims {..}

instance (ToJSON fct, ToJSON rsc, ToJSON ptc) => ToJSON (Claims fct rsc ptc) where
  toJSON Claims {..} = object
    [ "iss" .= sender
    , "aud" .= receiver
    --
    , "prf" .= proof
    , "ptc" .= potency
    , "rsc" .= resource
    , "fct" .= facts
    --
    , "nbf" .= toSeconds nbf
    , "exp" .= toSeconds exp
    ]

instance (FromJSON fct, FromJSON rsc, FromJSON ptc) => FromJSON (Claims fct rsc ptc) where
  parseJSON = withObject "JWT.Payload" \obj -> do
    sender   <- obj .: "iss"
    receiver <- obj .: "aud"
    --
    resource <- obj .:  "rsc" .!= Nothing
    potency  <- obj .:  "ptc" .!= Nothing
    proof    <- obj .:? "prf" .!= RootCredential
    facts    <- obj .:? "fct" .!= []
    --
    nbf <- fromSeconds <$> obj .: "nbf"
    exp <- fromSeconds <$> obj .: "exp"

    return Claims {..}

-----------
-- Proof --
-----------

data Proof fct rsc ptc
  = RootCredential
  | Nested    UCAN.RawContent (UCAN fct rsc ptc)
  | Reference CID
  deriving (Show, Eq)

instance
  ( Arbitrary fct
  , Arbitrary rsc
  , Arbitrary ptc
  , ToJSON fct
  , ToJSON rsc
  , ToJSON ptc
  ) => Arbitrary (Proof fct rsc ptc) where
  arbitrary = frequency
    [ (1, nested)
    , (5, pure RootCredential)
    ]
    where
      nested = do
        innerUCAN@(UCAN {..}) <- arbitrary
        let rawContent = RawContent $ B64.URL.encodeJWT header claims
        return $ Nested rawContent innerUCAN

instance Display (Proof fct rsc ptc) where
  display = \case
    RootCredential -> "RootCredential"
    Nested raw _   -> "Nested "    <> display raw
    Reference cid  -> "Reference " <> display cid

instance (ToJSON fct, ToJSON rsc, ToJSON ptc) => ToJSON (Proof fct rsc ptc) where
  toJSON = \case
    RootCredential ->
      Null

    Reference cid ->
      toJSON cid

    Nested (UCAN.RawContent raw) UCAN {sig} ->
      String (raw <> "." <> textDisplay sig)

instance (FromJSON fct, FromJSON rsc, FromJSON ptc) => FromJSON (Proof fct rsc ptc) where
  parseJSON Null = return RootCredential
  parseJSON val  = withText "Credential Proof" resolver val
    where
      resolver txt =
        if "eyJ" `Text.isPrefixOf` txt -- i.e. starts with Base64 encoded '{'
          then Nested (UCAN.contentOf txt) <$> parseJSON val
          else Reference <$> parseJSON val

-----------------------
-- Signature Helpers --
-----------------------

signEd25519 ::
  ( ToJSON fct
  , ToJSON rsc
  , ToJSON ptc
  )
  => Header
  -> Claims fct rsc ptc
  -> Ed25519.SecretKey
  -> Signature.Signature
signEd25519 header claims sk =
  Signature.Ed25519 . Key.signWith sk . encodeUtf8 $ B64.URL.encodeJWT header claims

signRS256 ::
  ( MonadRandom m
  , ToJSON fct
  , ToJSON rsc
  , ToJSON ptc
  )
  => Header
  -> Claims fct rsc ptc
  -> RSA.PrivateKey
  -> m (Either RSA.Error Signature.Signature)
signRS256 header claims sk =
  RSA.PKCS15.signSafer (Just SHA256) sk (encodeUtf8 $ B64.URL.encodeJWT header claims) <&> \case
    Left err  -> Left err
    Right sig -> Right . Signature.RS256 $ RS256.Signature sig

signSecp256k1 :: ( ToJSON fct
  , ToJSON rsc
  , ToJSON ptc
  )
  => Header
  -> Claims fct rsc ptc
  -> Secp256k1.SecKey
  -> Maybe Signature.Signature
signSecp256k1 header claims sk =
  -- Only works if the signed data is 32 bytes
  Signature.Secp256k1 . Secp256k1.Signature . Secp256k1.exportRecoverableSignature <$> Secp256k1.ecdsaSignRecoverable sk (encodeUtf8 $ B64.URL.encodeJWT header claims)
