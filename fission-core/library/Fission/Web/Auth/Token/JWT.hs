module Fission.Web.Auth.Token.JWT
  ( UCAN   (..)
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

import qualified Data.Aeson                                       as JSON

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

-- FIXME delete import           Fission.Authorization.Potency.Types
import           Fission.User.DID.Types
import           Fission.User.Username.Types

import           Fission.URL.DomainName.Types
import           Fission.URL.Types

import           Fission.Web.Auth.Token.JWT.Header.Types          (Header (..))
import qualified Fission.Web.Auth.Token.JWT.RawContent            as JWT
import           Fission.Web.Auth.Token.JWT.Signature             as Signature
import qualified Fission.Web.Auth.Token.JWT.Signature.RS256.Types as RS256

-- FIXME delete import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
-- FIXME delete import           Fission.Web.Auth.Token.UCAN.Resource.Types

-- Reexports

import           Fission.Web.Auth.Token.JWT.RawContent

-- Orphans

import           Fission.Internal.Orphanage.CID                   ()
import           Fission.Internal.Orphanage.Ed25519.SecretKey     ()

-- | An RFC 7519 extended with support for Ed25519 keys,
--     and some specifics (claims, etc) for Fission's use case
data UCAN = UCAN
  { header :: !Header
  , claims :: !Claims
  , sig    :: !Signature.Signature
  } deriving (Show, Eq)

instance Arbitrary UCAN where
  arbitrary = do
    header   <- arbitrary
    (pk, sk) <- case alg header of
      Algorithm.RSA2048 -> do
        RSA2048.Pair pk' sk' <- arbitrary
        return (RSAPublicKey pk', Left sk')

      Algorithm.EdDSA -> do
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
      Right sig -> return UCAN {..}

instance ToJSON UCAN where
  toJSON UCAN {..} = String $ content <> "." <> textDisplay sig
    where
      content = decodeUtf8Lenient $ encodeB64 header <> "." <> encodeB64 claims

      encodeB64 jsonable =
        jsonable
          |> encode
          |> Lazy.toStrict
          |> UTF8.stripQuotesBS
          |> BS.B64.URL.encode
          |> UTF8.stripPadding

instance FromJSON UCAN where
  parseJSON = withText "JWT.Token" \txt ->
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

------------
-- Claims --
------------

data Attenuation
  = FileSystem WNFSAttenuation
  deriving (Show, Eq)

instance Arbitrary Attenuation where
  arbitrary = FileSystem <$> arbitrary

instance ToJSON Attenuation where
  toJSON (FileSystem att) = toJSON att

instance FromJSON Attenuation where
  parseJSON val = FileSystem <$> parseJSON val

data WNFSAttenuation = WNFSAttenuation
  { wnfsResource :: !WNFSResource
  , capability   :: !WNFSCapability
  }
  deriving (Show, Eq)

instance Arbitrary WNFSAttenuation where
  arbitrary = do
    wnfsResource <- arbitrary
    capability   <- arbitrary

    return WNFSAttenuation {..}

instance ToJSON WNFSAttenuation where
  toJSON WNFSAttenuation {..} =
    object
      [ "wnfs" .= urn
      , "cap"  .= capability
      ]
    where
      urn :: WNFSResource
      urn = undefined -- FIXME

instance FromJSON WNFSAttenuation where
  parseJSON = withObject "WNFS.Attenuation" \obj -> do
    wnfsResource <- obj .: "wnfs"
    capability   <- obj .: "cap"

    return WNFSAttenuation {..}

data WNFSResource = WNFSResource
  { namespace :: DomainName
  , username  :: Username
  , filePath  :: FilePath
  }
  deriving (Show, Eq)

instance Arbitrary WNFSResource where
  arbitrary = do
    namespace  <- arbitrary -- FIXME may need some more constraint
    username   <- arbitrary
    filePath   <- arbitrary

    return WNFSResource {..}

instance ToJSON WNFSResource where
  toJSON WNFSResource {..} =
    String (textDisplay username <> "." <> textDisplay namespace <> normalizedPath)
    where
      txt =
        Text.pack filePath

      normalizedPath =
        case Text.uncons txt of
         Just ('/', _) -> txt
         _             -> "/" <> txt

instance FromJSON WNFSResource where
  parseJSON = withText "WNFS.Path" \txt -> do
    let
      (url, path') =
        Text.break (/= '/') txt

      pathStr =
        Text.unpack path'

      filePath =
        case pathStr of
         ('/' : _) -> pathStr
         _         -> '/' : pathStr

    URL
      { subdomain = Just (Subdomain rawSubdomain)
      , domainName
      } <- parseJSON $ String url

    username <- parseJSON $ String rawSubdomain

    return WNFSResource
      { namespace = domainName
      , username
      , filePath
      }

data WNFSCapability
  = Create
  | Revise
  | SoftDelete
  | Overwrite
  | SuperUser
  deriving (Show, Eq, Ord)

instance Arbitrary WNFSCapability where
  arbitrary = elements
    [ Create
    , Revise
    , SoftDelete
    , Overwrite
    , SuperUser
    ]

instance ToJSON WNFSCapability where
  toJSON = \case
    Create     -> "CREATE"
    Revise     -> "REVISE"
    SoftDelete -> "SOFT_DELETE"
    Overwrite  -> "OVERWRITE"
    SuperUser  -> "SUPER_USER"

instance FromJSON WNFSCapability where
  parseJSON = withText "WNFS.Capability" \txt ->
    case Text.toUpper txt of
      "CREATE"      -> pure Create
      "REVISE"      -> pure Revise
      "SOFT_DELETE" -> pure SoftDelete
      "OVERWRITE"   -> pure Overwrite
      "SUPER_USER"  -> pure SuperUser
      other         -> fail $ show other <> " is not a valid capabilty"

newtype Fact = Fact JSON.Value
  deriving newtype ( Eq
                   , Show
                   , IsString
                   , ToJSON
                   , FromJSON
                   )

instance Arbitrary Fact where
  arbitrary = elements ["hello world"] -- FIXME Expand

data Claims = Claims
  -- Dramatis Personae
  { sender       :: !DID
  , receiver     :: !DID
  -- Scope (set-like operations)
  , proofs       :: !Proof
  , attenuations :: ![Attenuation]
  -- Additional signed info
  , facts        :: ![Fact]
  -- Temporal Bounds
  , exp          :: !UTCTime
  , nbf          :: !UTCTime
  } deriving Show

instance Display Claims where
  textDisplay = Text.pack . show

instance Eq Claims where
  ucanA == ucanB = eqWho && eqAuth && eqTime
    where
      eqWho = sender ucanA == sender   ucanB
         && receiver ucanA == receiver ucanB

      eqAuth = attenuations ucanA == attenuations ucanB
             &&      proofs ucanA == proofs       ucanB
             &&       facts ucanA == facts        ucanB

      eqTime = roundUTC (exp ucanA) == roundUTC (exp ucanB)
            && roundUTC (nbf ucanA) == roundUTC (nbf ucanB)

instance Arbitrary Claims where
  arbitrary = do
    sender     <- arbitrary
    receiverPK <- arbitrary

    exp <- arbitrary
    nbf <- arbitrary

    attenuations <- arbitrary
    proofs       <- arbitrary
    facts        <- arbitrary

    let
      receiver = DID
        { publicKey = receiverPK
        , method    = Key
        }

    return Claims {..}

instance ToJSON Claims where
  toJSON Claims {..} = object
    [ "iss" .= sender
    , "aud" .= receiver
    --
    , "prf" .= proofs
    , "att" .= attenuations
    , "fct" .= facts
    --
    , "nbf" .= toSeconds nbf
    , "exp" .= toSeconds exp
    ]

instance FromJSON Claims where
  parseJSON = withObject "JWT.Payload" \obj -> do
    sender   <- obj .: "iss"
    receiver <- obj .: "aud"
    --
    attenuations <- obj .:  "att"
    proofs'      <- obj .:? "prf"
    facts        <- obj .:? "fct" .!= []
    --
    nbf <- fromSeconds <$> obj .: "nbf"
    exp <- fromSeconds <$> obj .: "exp"

    let
      proofs =
        case proofs' of
          Nothing   -> RootCredential
          Just []   -> RootCredential
          Just prfs -> DelegatedFrom prfs

    return Claims {..}

-----------
-- Proof --
-----------

data Proof
  = RootCredential
  | DelegatedFrom [DelegateProof]
  deriving (Show, Eq)

data DelegateProof
  = Nested    JWT.RawContent UCAN
  | Reference CID
  deriving (Show, Eq)

instance Arbitrary Proof where
  arbitrary = frequency
    [ (1, DelegatedFrom <$> nested)
    , (5, pure RootCredential)
    ]
    where
      nested :: Gen [DelegateProof]
      nested = do
        innerJWT@(UCAN {..}) <- arbitrary
        let rawContent = RawContent $ B64.URL.encodeJWT header claims
        return [Nested rawContent innerJWT]

instance ToJSON Proof where
  toJSON RootCredential        = Null
  toJSON (DelegatedFrom inner) = toJSON inner

instance ToJSON DelegateProof where
  toJSON = \case
    Reference cid ->
      toJSON cid

    Nested (JWT.RawContent raw) UCAN {sig} ->
      String (raw <> "." <> textDisplay sig)

instance FromJSON Proof where
  parseJSON Null = return RootCredential
  parseJSON val  = DelegatedFrom <$> parseJSON val

instance FromJSON DelegateProof where
  parseJSON val = withText "Delegate Proof" resolver val
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
