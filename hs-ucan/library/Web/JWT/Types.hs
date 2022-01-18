module Web.JWT.Types
  ( JWT    (..)
  , Claims (..)
  , Proof  (..)

  , signEd25519
  , signRS256

  -- * reexports

  , module Web.JWT.Header.Types
  , module Web.JWT.RawContent
  ) where

import qualified System.IO.Unsafe                          as Unsafe

import           Crypto.Hash.Algorithms                    (SHA256 (..))
import           Crypto.Random                             (MonadRandom (..))

import qualified Crypto.PubKey.RSA                         as RSA
import qualified Crypto.PubKey.RSA.PKCS15                  as RSA.PKCS15

import           Crypto.PubKey.Ed25519                     (toPublic)
import qualified Crypto.PubKey.Ed25519                     as Ed25519

import           Data.Aeson
import qualified Data.ByteString.Base64.URL                as BS.B64.URL

import           Network.IPFS.CID.Types

import qualified RIO.ByteString.Lazy                       as Lazy
import qualified RIO.Text                                  as Text

import qualified Servant.API                               as Servant

import           RIO hiding (exp)
import RIO.Time


import           Test.QuickCheck

import           Crypto.Key.Asymmetric                     as Key

import qualified Crypto.Key.Asymmetric.Algorithm.Types     as Algorithm
import qualified Crypto.Key.Asymmetric.RSA2048.Pair.Types  as RSA2048
import qualified Ucan.Internal.Base64.URL                  as B64.URL
import qualified Ucan.Internal.UTF8                        as UTF8
import Ucan.Internal.Time

import           Web.DID.Types
import           Web.JWT.Potency.Types

import           Web.JWT.Header.Types                      (Header (..))
import qualified Web.JWT.RawContent                        as JWT
import           Web.JWT.Signature                         as Signature
import qualified Web.JWT.Signature.RS256.Types             as RS256

import           Ucan.Internal.Orphanage.Ed25519.SecretKey ()

-- Reexports

import           Web.JWT.RawContent

-- | An RFC 7519 extended with support for Ed25519 keys,
--     and some specifics (claims, etc) for Fission's use case
data JWT fct rsc = JWT
  { header :: Header
  , claims :: Claims fct rsc
  , sig    :: Signature.Signature
  } deriving (Show, Eq)

instance
  ( Arbitrary fct
  , Arbitrary rsc
  , ToJSON fct
  , ToJSON rsc
  ) => Arbitrary (JWT fct rsc) where
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

instance (ToJSON fct, ToJSON rsc) => ToJSON (JWT fct rsc) where
  toJSON JWT {..} = String $ content <> "." <> textDisplay sig
    where
      content = decodeUtf8Lenient $ encodeB64 header <> "." <> encodeB64 claims

      encodeB64 jsonable =
        jsonable
          & encode
          & Lazy.toStrict
          & UTF8.stripQuotesBS
          & BS.B64.URL.encode
          & UTF8.stripPadding

instance (FromJSON fct, FromJSON rsc) => FromJSON (JWT fct rsc) where
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

instance
  ( Servant.ToHttpApiData fct
  , Servant.ToHttpApiData rsc
  , ToJSON fct
  , ToJSON rsc
  ) => Servant.ToHttpApiData (JWT fct rsc) where
  toUrlPiece jwt =
    jwt
      & encode
      & Lazy.toStrict
      & decodeUtf8Lenient
      & UTF8.stripQuotes

------------
-- Claims --
------------

data Claims fct rsc = Claims
  -- Dramatis Personae
  { sender   :: DID
  , receiver :: DID
  -- Authorization Target
  , resource :: Maybe rsc
  , potency  :: Potency
  , proof    :: Proof fct rsc
  -- 0.3.1
  , facts    :: [fct]
  -- Temporal Bounds
  , exp      :: UTCTime
  , nbf      :: UTCTime
  } deriving Show

instance (Show fct, Show rsc) => Display (Claims fct rsc) where
  textDisplay = Text.pack . show

instance (Eq fct, Eq rsc) => Eq (Claims fct rsc) where
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
  , ToJSON fct
  , ToJSON rsc
  ) => Arbitrary (Claims fct rsc) where
  arbitrary = do
    sender   <- arbitrary
    resource <- arbitrary
    potency  <- arbitrary
    proof    <- arbitrary
    facts    <- arbitrary
    exp      <- arbitrary
    nbf      <- arbitrary
    pk       <- arbitrary

    let
      receiver = DID
        { publicKey = pk
        , method    = Key
        }

    return Claims {..}

instance (ToJSON fct, ToJSON rsc) => ToJSON (Claims fct rsc) where
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

instance (FromJSON fct, FromJSON rsc) => FromJSON (Claims fct rsc) where
  parseJSON = withObject "JWT.Payload" \obj -> do
    sender   <- obj .: "iss"
    receiver <- obj .: "aud"
    --
    resource <- obj .:  "rsc" .!= Nothing
    potency  <- obj .:? "ptc" .!= AuthNOnly
    proof    <- obj .:? "prf" .!= RootCredential
    facts    <- obj .:? "fct" .!= []
    --
    nbf <- fromSeconds <$> obj .: "nbf"
    exp <- fromSeconds <$> obj .: "exp"

    return Claims {..}

-----------
-- Proof --
-----------

data Proof fct rsc
  = RootCredential
  | Nested    JWT.RawContent (JWT fct rsc)
  | Reference CID
  deriving (Show, Eq)

instance
  ( Arbitrary fct
  , Arbitrary rsc
  , ToJSON fct
  , ToJSON rsc
  ) => Arbitrary (Proof fct rsc) where
  arbitrary = frequency
    [ (1, nested)
    , (5, pure RootCredential)
    ]
    where
      nested = do
        innerJWT@(JWT {..}) <- arbitrary
        let rawContent = RawContent $ B64.URL.encodeJWT header claims
        return $ Nested rawContent innerJWT

instance (Display fct, Display rsc) => Display (Proof fct rsc) where
  display = \case
    RootCredential -> "RootCredential"
    Nested raw _   -> "Nested "    <> display raw
    Reference cid  -> "Reference " <> display cid

instance (ToJSON fct, ToJSON rsc) => ToJSON (Proof fct rsc) where
  toJSON = \case
    RootCredential ->
      Null

    Reference cid ->
      toJSON cid

    Nested (JWT.RawContent raw) JWT {sig} ->
      String (raw <> "." <> textDisplay sig)

instance (FromJSON fct, FromJSON rsc) => FromJSON (Proof fct rsc) where
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

signEd25519 :: (ToJSON fct, ToJSON rsc) => Header -> Claims fct rsc -> Ed25519.SecretKey -> Signature.Signature
signEd25519 header claims sk =
  Signature.Ed25519 . Key.signWith sk . encodeUtf8 $ B64.URL.encodeJWT header claims

signRS256 ::
  ( MonadRandom m
  , ToJSON fct
  , ToJSON rsc
  )
  => Header
  -> Claims fct rsc
  -> RSA.PrivateKey
  -> m (Either RSA.Error Signature.Signature)
signRS256 header claims sk =
  RSA.PKCS15.signSafer (Just SHA256) sk (encodeUtf8 $ B64.URL.encodeJWT header claims) <&> \case
    Left err  -> Left err
    Right sig -> Right . Signature.RS256 $ RS256.Signature sig
