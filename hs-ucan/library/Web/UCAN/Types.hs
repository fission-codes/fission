{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Types
  ( UCAN (..)
  , Claims (..)
  , Witness  (..)
  , Capability (..)
  , OwnedResources (..)
  , OwnershipScope (..)
  , Ability (..)
  , ProofRedelegation (..)

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

import qualified Text.URI                                      as URI

import qualified Data.Attoparsec.Text                          as Parse

import           Network.IPFS.CID.Types

import           Control.Monad                                 (replicateM)
import           RIO                                           hiding (exp)
import qualified RIO.ByteString.Lazy                           as Lazy
import qualified RIO.Char                                      as Char
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
    algorithm <- frequency
      -- RSA Signatures & keys are huge and slow down tests a *lot*
      [ (1, pure Algorithm.RSA2048)
      , (5, pure Algorithm.Ed25519)
      ]
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
    proofs      <- map (Nested . textDisplay) <$> sized arbitraryWitness
    facts       <- arbitrary
    expiration  <- arbitrary
    notBefore   <- arbitrary
    nonce       <- arbitrary

    return Claims {..}
    where
      distributeRands :: Int -> Int -> Gen [Int]
      distributeRands 0 _ = return []
      distributeRands numChildren available = do
        chosen <- choose (0, available)
        rest <- distributeRands (numChildren - 1) (available - chosen)
        return (chosen:rest)

      arbitraryWitness :: Int -> Gen [UCAN fct res abl]
      arbitraryWitness size = do
        numChildren <- choose (0, size)
        distributedSizes <- distributeRands numChildren size
        mapM (\childSize -> resize childSize arbitrary) distributedSizes


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
    , "att" .= attenuation
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
    attenuation <- obj .:? "att" .!= []
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
  deriving (Show, Eq, Ord)

-- | The "wnfs/APPEND" part of a capability as in { with: "wnfs://...", can: "wnfs/APPEND" }
data Ability ability
  = SuperUser       -- ^ represents can: "*"
  | Ability ability -- ^ represents any other can: "scope/action" pair
  deriving (Show, Eq, Ord)

-- | Represents resources of the form "my:<ability>" or "as:<did>:<ability>" and their associated ability
data OwnedResources ability
  = OwnedResources (Maybe DID) (OwnershipScope ability)
  deriving (Show, Eq, Ord)

-- | Represents the set of abilities referred to in "my:<ability>"
-- | or "as:<did>:<ability>" capabilities.
data OwnershipScope ability
  = All
    -- ^ the "*" in "my:*". The whole capability *must* be "{ with: "my:*", can: "*" }" or the equivalent with "as:..."
  | OnlyScheme (URI.RText 'URI.Scheme) ability
    -- ^ e.g. the "wnfs" in "my:wnfs" or "as:<did>:wnfs"
  deriving (Show, Eq, Ord)

-- | Represents the "with" in e.g. { with: "prf:*", can: "ucan/DELEGATE" }
data ProofRedelegation
  = RedelegateAllProofs     -- ^ prf:*
  | RedelegateProof Natural -- ^ e.g. prf:0 or prf:10
  deriving (Show, Eq, Ord)


instance DelegationSemantics ability => DelegationSemantics (OwnershipScope ability) where
  All `canDelegate` _   = True
  _   `canDelegate` All = False
  (OnlyScheme aScheme aAbility) `canDelegate` (OnlyScheme bScheme bAbility) =
    aScheme == bScheme && aAbility `canDelegate` bAbility

instance DelegationSemantics ability => DelegationSemantics (Ability ability) where
  SuperUser   `canDelegate` _           = True
  _           `canDelegate` SuperUser   = False
  (Ability a) `canDelegate` (Ability b) = a `canDelegate` b

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

instance Arbitrary ability => Arbitrary (OwnedResources ability) where
  arbitrary = OwnedResources <$> arbitrary <*> arbitrary

instance Arbitrary ability => Arbitrary (OwnershipScope ability) where
  arbitrary =
    oneof
      [ pure All
      , OnlyScheme <$> arbitrary <*> arbitrary
      ]

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
  textDisplay SuperUser         = "*"
  textDisplay (Ability ability) = textDisplay ability

instance Arbitrary ability => Arbitrary (Ability ability) where
  arbitrary =
    frequency
      [ (5, return SuperUser)
      , (1, Ability <$> arbitrary)
      ]

instance FromJSON abl => FromJSON (Ability abl) where
  parseJSON value = value & withText "UCAN.Ability" \can -> do
    if can == "*"
      then return SuperUser
      else Ability <$> parseJSON value

instance Display ability => ToJSON (OwnedResources ability) where
  toJSON (OwnedResources maybeDID All) = object
    [ "with" .= String (myOrAsPrefix maybeDID <> "*")
    , "can"  .= String "*"
    ]
  toJSON (OwnedResources maybeDID (OnlyScheme scheme ability)) = object
    [ "with" .= String (myOrAsPrefix maybeDID <> URI.unRText scheme)
    , "can"  .= String (textDisplay ability)
    ]

myOrAsPrefix :: Maybe DID -> Text
myOrAsPrefix = \case
  Just did -> "as:" <> textDisplay did <> ":"
  Nothing  -> "my:"


instance (Display res, Display abl) => ToJSON (Capability res abl) where
  toJSON = \case
    CapResource res ability ->
      object
        [ "with" .= String (textDisplay res)
        , "can"  .= String (textDisplay ability)
        ]

    CapOwnedResources owned ->
      toJSON owned

    CapProofRedelegation redel ->
      toJSON redel

data AsOrMyOrPrf
  = My (Maybe (URI.RText 'URI.Scheme))
  | As DID (Maybe (URI.RText 'URI.Scheme))
  | Prf (Maybe Natural)

parseScheme :: Parse.Parser (URI.RText 'URI.Scheme)
parseScheme = do
  x <- Parse.satisfy (\c -> Char.isAscii c || Char.isAlpha c)
  xs <- Parse.takeWhile (\c -> Char.isAscii c || Char.isAlphaNum c || c `elem` ("+-." :: String))
  case URI.mkScheme $ Text.singleton x <> xs of
    Right scheme   -> return scheme
    Left exception -> fail $ "Can't parse scheme: " <> show exception

parseStarOr :: Parse.Parser a -> Parse.Parser (Maybe a)
parseStarOr alternative =
  let parseStar = fmap (const Nothing) $ Parse.char '*'
  in fmap Just alternative <|> parseStar


data MAP = M | A | P

parseAsOrMyOrPrf :: Parse.Parser AsOrMyOrPrf
parseAsOrMyOrPrf = do
  prefix <- Parse.string "my:"  *> return M
        <|> Parse.string "as:"  *> return A
        <|> Parse.string "prf:" *> return P
  case prefix of
    P -> Prf <$> parseStarOr Parse.decimal
    M -> My  <$> parseStarOr parseScheme
    A -> As  <$> DID.parse <* Parse.string ":" <*> parseStarOr parseScheme


instance (FromJSON res, FromJSON abl) => FromJSON (Capability res abl) where
  parseJSON = withObject "UCAN.Capability" \obj -> do
    withField <- obj .: "with"
    canField  <- obj .: "can"

    case (withField, canField) of
      (String with, String can) ->
        case Parse.parseOnly (parseAsOrMyOrPrf <* Parse.endOfInput) with of
          Right (My Nothing) -> do
            when (can /= "*") do
              fail "The 'my:*' resource must have the ability set to '*'"

            return $ CapOwnedResources $ OwnedResources Nothing All

          Right (As did Nothing) -> do
            when (can /= "*") do
              fail "They 'as:<did>:*' resource must have the ability set to '*'"

            return $ CapOwnedResources $ OwnedResources (Just did) All

          Right (My (Just scheme)) -> do
            -- TODO Figure out exactly how to slice this problem. Where do we allow the ability parser to parse?
            ability <- parseJSON canField
            return $ CapOwnedResources $ OwnedResources Nothing (OnlyScheme scheme ability)

          Right (As did (Just scheme)) -> do
            -- TODO see above
            ability <- parseJSON canField
            return $ CapOwnedResources $ OwnedResources (Just did) (OnlyScheme scheme ability)

          Right (Prf idx) -> do
            when (Text.toLower can /= "ucan/delegate") do
              fail "The 'prf' resource must have the ability set to 'ucan/DELEGATE'"

            return $ CapProofRedelegation $ maybe RedelegateAllProofs RedelegateProof idx

          Left _ -> do
            -- TODO see above
            resource <- parseJSON withField
            ability <- parseJSON canField
            return $ CapResource resource ability

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
  , Display res
  , Display abl
  )
  => Ed25519.SecretKey
  -> Claims fct res abl
  -> UCAN fct res abl
signEd25519 sk claims = UCAN{..}
  where
    header     = Header { typ = Typ.JWT, alg = Algorithm.Ed25519, ucv = ucanVersion }
    raw        = B64.URL.encodeJWT header (jsonFromClaims claims)
    signedData = RawContent raw
    signature  = Signature.Ed25519 $ Key.signWith sk $ encodeUtf8 raw

signRS256 ::
  ( MonadRandom m
  , ToJSON fct
  , Display res
  , Display abl
  )
  => RSA.PrivateKey
  -> Claims fct res abl
  -> m (Either RSA.Error (UCAN fct res abl))
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
  forall fct rsc abl .
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
      case proof of
        Nothing ->
          return [CapOwnedResources (OwnedResources (Just sender) All)]

        Just (Nested jwt) ->
          case decodeStrict @(UCAN fct rsc abl) $ Text.encodeUtf8 jwt of
            Nothing ->
              return []

            Just ucan ->
              case attenuation $ claims ucan  of
                [CapOwnedResources res] ->
                  return [CapOwnedResources res]

                _ ->
                  return []

        _ ->
          return []

    (Just rsc, Just pot) -> do
      -- TODO Somewhat of a hack to support backwards compatibility for capabilities
      -- probably needs special-casing for a WNFS capability
      let cap = object
            [ ("rsc", rsc)
            , ("pot", pot)
            ]
      res <- parseJSON cap
      abl <- parseJSON cap
      return [CapResource res abl]

    _ ->
      return []

  return Claims
    { sender = sender
    , receiver = receiver
    , attenuation = attenuation
    , proofs = maybeToList proof
    , facts = facts
    , notBefore = Just notBefore
    , expiration = expiration
    , nonce = Nothing
    }
