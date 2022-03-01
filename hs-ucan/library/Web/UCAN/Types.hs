{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Types
  ( UCAN (..)
  , Claims (..)
  , Witness  (..)

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

import           Data.Aeson
import qualified Data.Aeson.Types                              as JSON
import qualified Data.ByteString.Base64.URL                    as BS.B64.URL
import qualified Data.Text.Encoding.Base64.URL                 as Text.B64.URL

import           Text.URI                                      (URI)
import qualified Text.URI                                      as URI

import qualified Data.Attoparsec.Text                          as Parse

import           Network.IPFS.CID.Types

import           Control.Monad                                 (replicateM)
import           RIO                                           hiding (exp)
import qualified RIO.ByteString.Lazy                           as Lazy
import qualified RIO.Text                                      as Text
import           RIO.Time

import qualified Servant.API                                   as Servant


import           Test.QuickCheck


import           Crypto.Key.Asymmetric                         as Key
import qualified Crypto.Key.Asymmetric.Algorithm.Types         as Algorithm

import           Web.DID.Types                                 as DID

import           Web.SemVer.Types
import qualified Web.UCAN.Header.Typ.Types                     as Typ
import           Web.UCAN.Witness.Class

import           Web.UCAN.Signature                            as Signature
import qualified Web.UCAN.Signature.RS256.Types                as RS256

import qualified Web.UCAN.Internal.Base64.URL                  as B64.URL
import           Web.UCAN.Internal.Orphanage.Ed25519.SecretKey ()
import qualified Web.UCAN.Internal.RSA2048.Pair.Types          as RSA2048
import           Web.UCAN.Internal.Time
import qualified Web.UCAN.Internal.UTF8                        as UTF8


-- Reexports

import           Web.UCAN.Header.Types
import           Web.UCAN.RawContent


-- | An RFC 7519 extended with support for Ed25519 keys,
--     and some specifics (claims, etc) for Fission's use case
data UCAN fct res abl = UCAN
  { header     :: Header
  , claims     :: Claims fct res abl
  , signedData :: RawContent
  , signature  :: Signature.Signature
  } deriving (Show, Eq)


instance Display (UCAN fct res abl) where
  textDisplay UCAN{..} =
    textDisplay signedData <> "." <> textDisplay signature


instance
  ( Arbitrary fct
  , Arbitrary res
  , Arbitrary abl
  , ToJSON fct
  , Display res
  , Display abl
  ) => Arbitrary (UCAN fct res abl) where
  arbitrary = do
    algorithm <- elements [Algorithm.RSA2048, Algorithm.Ed25519]
    (pk, sk) <- case algorithm of
      Algorithm.RSA2048 -> do
        RSA2048.Pair pk' sk' <- arbitrary
        return (RSAPublicKey pk', Left sk')

      Algorithm.Ed25519 -> do
        sk' <- arbitrary
        return (Ed25519PublicKey (toPublic sk'), Right sk')

    claims' <- arbitrary

    let claims = claims' {sender = DID.Key pk}

    return case sk of
      Left rsaSK ->
        case Unsafe.unsafePerformIO $ signRS256 rsaSK claims of
          Left err   -> error $ "Unable to sign UCAN: " <> show err
          Right ucan -> ucan

      Right edSK ->
        signEd25519 edSK claims

instance
  ( ToJSON fct
  , Display res
  , Display abl
  ) => ToJSON (UCAN fct res abl) where
  toJSON UCAN {..} = String $ content <> "." <> textDisplay signature
    where
      content = decodeUtf8Lenient $ encodeB64 header <> "." <> encodeB64 (jsonFromClaims claims)

      encodeB64 jsonable =
        jsonable
          & encode
          & Lazy.toStrict
          & UTF8.stripQuotesBS
          & BS.B64.URL.encodeBase64'
          & UTF8.stripPadding

instance
  ( FromJSON fct
  , FromJSON res
  , FromJSON abl
  ) => FromJSON (UCAN fct res abl) where
  parseJSON = withText "UCAN.Token" \txt ->
    case Text.split (== '.') txt of
      [rawHeader, rawClaims, rawSig] -> do
        header <- withEmbeddedJSON "Header" parseJSON $ jsonify rawHeader

        let parseClaims = case ucv header of
              SemVer 0 3 _ -> parseClaimsV_0_3
              _            -> parseJSON

        claims <- withEmbeddedJSON "Claims" parseClaims $ jsonify rawClaims

        let
          raw = rawHeader <> "." <> rawClaims
          signedData = RawContent raw
        signature <- Signature.parse (alg header) (toJSON rawSig)

        return UCAN {..}

      _ ->
        fail $ "Wrong number of JWT segments in:  " <> Text.unpack txt
    where
      jsonify = toJSON . Text.B64.URL.decodeBase64Lenient

instance
  ( ToJSON fct
  , Display res
  , Display abl
  ) => Servant.ToHttpApiData (UCAN fct res abl) where
  toUrlPiece ucan =
    ucan
      & encode
      & Lazy.toStrict
      & decodeUtf8Lenient
      & UTF8.stripQuotes

------------
-- Claims --
------------

data Claims fct cap abl = Claims
  -- Dramatis Personae
  { sender      :: DID
  , receiver    :: DID
  -- Authorization Target
  , attenuation :: [Capability cap abl]
  , proofs      :: [Witness]
  , facts       :: [fct]
  -- Temporal Bounds
  , expiration  :: UTCTime
  , notBefore   :: Maybe UTCTime
  , nonce       :: Maybe Nonce
  } deriving (Show)

instance (Show fct, Show cap, Show abl) => Display (Claims fct cap abl) where
  textDisplay = Text.pack . show

instance (Eq fct, Eq cap, Eq abl) => Eq (Claims fct cap abl) where
  jwtA == jwtB = eqWho && eqAuth && eqTime && eqFacts && eqNonce
    where
      eqWho = sender jwtA == sender   jwtB
         && receiver jwtA == receiver jwtB

      eqAuth = attenuation jwtA == attenuation jwtB
            &&      proofs jwtA == proofs      jwtB

      eqTime = roundUTC (expiration jwtA) ==      roundUTC (expiration jwtB)
       && fmap roundUTC (notBefore  jwtA) == fmap roundUTC (notBefore  jwtB)

      eqFacts = facts jwtA == facts jwtB

      eqNonce = nonce jwtA == nonce jwtB

instance
  ( Arbitrary fct
  , Arbitrary res
  , Arbitrary abl
  , ToJSON fct
  , Display res
  , Display abl
  ) => Arbitrary (Claims fct res abl) where
  arbitrary = do
    sender      <- arbitrary
    receiver    <- DID.Key <$> arbitrary
    attenuation <- arbitrary
    proofs      <- map (Nested . textDisplay) <$> arbitraryWitnesss
    facts       <- arbitrary
    expiration  <- arbitrary
    notBefore   <- arbitrary
    nonce       <- arbitrary

    return Claims {..}
    where
      arbitraryWitnesss :: Gen [UCAN fct res abl]
      arbitraryWitnesss =
        -- try to generate deep rather than wide UCANs
        frequency
          [ (1,  replicateM 2 arbitrary)
          , (4,  replicateM 1 arbitrary)
          , (20, replicateM 0 arbitrary)
          ]

jsonFromClaims ::
  ( ToJSON fct
  , Display res
  , Display abl
  ) => Claims fct res abl -> JSON.Value
jsonFromClaims Claims {..} = object
    [ "iss" .= sender
    , "aud" .= receiver
    --
    , "prf" .= proofs
    , "att" .= toJSON (map (jsonFromCapability sender) attenuation)
    , "fct" .= facts
    --
    , "exp" .=      toSeconds expiration
    , "nbf" .= fmap toSeconds notBefore
    , "nnc" .= nonce
    ]

instance
  ( FromJSON fct
  , FromJSON cap
  , FromJSON abl
  ) => FromJSON (Claims fct cap abl) where
  parseJSON = withObject "JWT.Payload" \obj -> do
    sender   <- obj .: "iss"
    receiver <- obj .: "aud"
    --
    attenuation <- JSON.explicitParseField (traverse (parseCapability sender)) obj "att" .!= []
    proofs      <- obj .:  "prf"
    facts       <- obj .:? "fct" .!= []
    --
    expiration <-      fromSeconds <$> obj .:  "exp"
    notBefore  <- fmap fromSeconds <$> obj .:? "nbf" .!= Nothing
    --
    nonce <- obj .:? "nnc" .!= Nothing

    return Claims {..}


------------------
-- Capabilities --
------------------

-- | A representation of capabilities in UCANs.
data Capability resource ability
  = CapResource resource (Ability ability)
  | CapOwnedResources (OwnedResources ability)
  | CapProofRedelegation ProofRedelegation
  deriving (Show, Eq)

-- | The "wnfs/APPEND" part in a capability, e.g. { with: "wnfs://...", can: "wnfs/APPEND" }
data Ability ability
  = SuperUser       -- ^ represents can: "*"
  | Ability ability -- ^ represents any other can: "scope/action" pair
  deriving (Show, Eq)

-- | Represents resources of the form "my:<ability>" or "as:<did>:<ability>".
-- | Both are represented as all resources from a particular DID.
-- | In case of "my:<ability>" this DID is taken from the UCAN's issuer that this is parsed from.
data OwnedResources ability
  = OwnedResources DID (OwnershipScope ability)
  deriving (Show, Eq)

-- | Represents the set of abilities referred to in "my:<ability>"
-- | or "as:<did>:<ability>" capabilities.
data OwnershipScope ability
  = All
    -- ^ the "*" in "my:*". The whole capability *must* be "{ with: "my:*", can: "*" }" or the equivalent with "as:..."
  | OnlyScheme (URI.RText 'URI.Scheme) ability
    -- ^ e.g. the "wnfs" in "my:wnfs" or "as:<did>:wnfs"
  deriving (Show, Eq)

-- | Represents the "with" in e.g. { with: "prf:*", can: "ucan/DELEGATE" }
data ProofRedelegation
  = RedelegateAllProofs     -- ^ prf:*
  | RedelegateProof Natural -- ^ e.g. prf:0 or prf:10
  deriving (Show, Eq)


instance DelegationSemantics ability => DelegationSemantics (OwnershipScope ability) where
  All `canDelegate` _   = True
  _   `canDelegate` All = False
  (OnlyScheme aScheme aAbility) `canDelegate` (OnlyScheme bScheme bAbility) =
    aScheme == bScheme && aAbility `canDelegate` bAbility

instance DelegationSemantics ability => DelegationSemantics (OwnedResources ability) where
  (OwnedResources aDID aScope) `canDelegate` (OwnedResources bDID bScope) =
    aDID == bDID && aScope `canDelegate` bScope

instance (Arbitrary res, Arbitrary abl) => Arbitrary (Capability res abl) where
  arbitrary =
    oneof
      [ CapResource <$> arbitrary <*> arbitrary
      , CapOwnedResources <$> arbitrary
      , CapProofRedelegation <$> arbitrary
      ]

instance Arbitrary ProofRedelegation where
  arbitrary =
    oneof
      [ pure RedelegateAllProofs
      , RedelegateProof <$> arbitrary
      ]

instance Arbitrary ability => Arbitrary (OwnershipScope ability) where
  arbitrary =
    oneof
      [ pure All
      , OnlyScheme <$> arbitrary <*> arbitrary
      ]

instance Arbitrary ability => Arbitrary (OwnedResources ability) where
  arbitrary = OwnedResources <$> arbitrary <*> arbitrary

instance ToJSON ProofRedelegation where
  toJSON RedelegateAllProofs = object
    [ "with" .= String "prf:*"
    , "can"  .= String "ucan/DELEGATE"
    ]
  toJSON (RedelegateProof n) = object
    [ "with" .= String ("prf:" <> Text.pack (show n))
    , "can"  .= String "ucan/DELEGATE"
    ]

instance Display ability => Display (Ability ability) where
  textDisplay SuperUser = "*"
  textDisplay (Ability ability) = textDisplay ability

instance Arbitrary ability => Arbitrary (Ability ability) where
  arbitrary =
    frequency
      [ (5, return SuperUser)
      , (1, Ability <$> arbitrary)
      ]

jsonFromCapability :: (Display res, Display abl) => DID -> Capability res abl -> JSON.Value
jsonFromCapability issuer = \case
  CapProofRedelegation redel ->
    toJSON redel

  CapResource res ability ->
    object
      [ "with" .= String (textDisplay res)
      , "can"  .= String (textDisplay ability)
      ]

  CapOwnedResources (OwnedResources did scope) ->
    object
      [ "with" .= String (myOrAs <> schemeOrStar)
      , "can"  .= case scope of
          All                  -> String "*"
          OnlyScheme _ ability -> String $ textDisplay ability
      ]
    where
      myOrAs = if did == issuer then "my:" else "as:" <> textDisplay did <> ":"
      schemeOrStar = case scope of
        All                 -> "*"
        OnlyScheme scheme _ -> URI.unRText scheme

parseCapability :: (FromJSON res, FromJSON abl) => DID -> JSON.Value -> JSON.Parser (Capability res abl)
parseCapability issuer = withObject "UCAN.Capability" \obj -> do
  withField <- obj .: "with"
  canField  <- obj .: "can"

  case (withField, canField) of
    (String "my:*", String "*") ->
      return $ CapOwnedResources $ OwnedResources issuer All

    (String "prf:*", String ucanDelegate ) | Text.toLower ucanDelegate == "ucan/delegate" ->
      return $ CapProofRedelegation RedelegateAllProofs

    (String my, String ability) | "my:" `Text.isPrefixOf` my ->
      undefined

    (String with, String can) ->
      undefined

    _ ->
      fail "'with' and 'can' fields must be strings"

-------------
-- Witness --
-------------

data Witness
  = Nested    Text
  | Reference CID
  deriving (Show, Eq)


instance Display Witness where
  display = \case
    Nested raw    -> "Nested "    <> display raw
    Reference cid -> "Reference " <> display cid

instance ToJSON Witness where
  toJSON = \case
    Reference cid ->
      toJSON cid

    Nested raw ->
      String raw

instance FromJSON Witness where
  parseJSON val =
    val
      & withText "Credential Witness" \txt ->
        if "eyJ" `Text.isPrefixOf` txt -- i.e. starts with Base64 encoded '{'
          then pure $ Nested txt
          else Reference <$> parseJSON val


-----------
-- Nonce --
-----------

newtype Nonce = Nonce { unNonce :: Text }
  deriving (Eq, Show, Ord)

instance Arbitrary Nonce where
  arbitrary = Nonce . Text.pack <$> replicateM 6 (elements chars)
    where
      chars =
        ['A'..'Z']
        <> ['a'..'z']
        <> ['0'..'9']

instance FromJSON Nonce where
  parseJSON = withText "UCAN Nonce" (pure . Nonce)

instance ToJSON Nonce where
  toJSON = String . unNonce



-----------------------
-- Signature Helpers --
-----------------------


signEd25519 ::
  ( ToJSON fct
  , ToJSON cap
  , Display abl
  )
  => Ed25519.SecretKey
  -> Claims fct cap abl
  -> UCAN fct cap abl
signEd25519 sk claims = UCAN{..}
  where
    header     = Header { typ = Typ.JWT, alg = Algorithm.Ed25519, ucv = ucanVersion }
    raw        = B64.URL.encodeJWT header (jsonFromClaims claims)
    signedData = RawContent raw
    signature  = Signature.Ed25519 $ Key.signWith sk $ encodeUtf8 raw

signRS256 ::
  ( MonadRandom m
  , ToJSON fct
  , ToJSON cap
  , Display abl
  )
  => RSA.PrivateKey
  -> Claims fct cap abl
  -> m (Either RSA.Error (UCAN fct cap abl))
signRS256 sk claims = do
  let raw = B64.URL.encodeJWT header (jsonFromClaims claims)
      signedData = RawContent raw
      header = Header { typ = Typ.JWT, alg = Algorithm.RSA2048, ucv = ucanVersion }
  sigResult <- RSA.PKCS15.signSafer (Just SHA256) sk (encodeUtf8 raw)
  case sigResult of
    Left err -> return $ Left err
    Right s  ->
      let signature = Signature.RS256 $ RS256.Signature s
      in return $ Right UCAN {..}


-----------------------------
-- Backwards-compatibility --
-----------------------------

parseClaimsV_0_3 ::
  ( FromJSON fct
  , FromJSON rsc
  , FromJSON abl
  ) => Value -> JSON.Parser (Claims fct rsc abl)
parseClaimsV_0_3 = withObject "JWT.Payload" \obj -> do
  sender   <- obj .: "iss"
  receiver <- obj .: "aud"
  --
  resource <- obj .:  "rsc" .!= Nothing
  potency  <- obj .:  "ptc" .!= Nothing
  proof    <- obj .:  "prf" .!= Nothing
  facts    <- obj .:? "fct" .!= []
  --
  expiration <- fromSeconds <$> obj .: "exp"
  notBefore  <- fromSeconds <$> obj .: "nbf"

  attenuation <- case (resource, potency) of
    (Just "*", _) ->
      case attenuation . claims <$> proof of
        Just [CapOwnedResources res] ->
          return [CapOwnedResources res]

        Nothing ->
          return [CapOwnedResources (OwnedResources sender All)]

        _ ->
          return []

    (Just rsc, Just pot) -> do
      -- TODO Somewhat of a hack to support backwards compatibility for capabilities
      -- probably needs special-casing for a WNFS capability
      let obj = object
            [ ("rsc", rsc)
            , ("pot", pot)
            ]
      resource <- parseJSON obj
      ability <- parseJSON obj
      return [CapResource resource ability]

    _ ->
      return []

  return Claims
    { sender = sender
    , receiver = receiver
    , attenuation = attenuation
    , proofs = [] -- TODO maybeToList proof
    , facts = facts
    , notBefore = Just notBefore
    , expiration = expiration
    , nonce = Nothing
    }
