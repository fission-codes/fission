{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Types
  ( UCAN (..)
  , Claims (..)
  , Witness  (..)

  , signEd25519
  , signRS256

  -- * reexports

  , module Web.UCAN.Capabilities.Types
  , module Web.UCAN.Claims.Types
  , module Web.UCAN.Header.Types
  , module Web.UCAN.Nonce.Types
  , module Web.UCAN.RawContent
  , module Web.UCAN.Witness.Types
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


import           RIO                                           hiding (exp)
import qualified RIO.ByteString.Lazy                           as Lazy
import qualified RIO.Text                                      as Text

import qualified Servant.API                                   as Servant


import           Test.QuickCheck


import           Crypto.Key.Asymmetric                         as Key
import qualified Crypto.Key.Asymmetric.Algorithm.Types         as Algorithm

import           Web.DID.Types                                 as DID

import           Web.SemVer.Types
import qualified Web.UCAN.Header.Typ.Types                     as Typ

import           Web.UCAN.Signature                            as Signature
import qualified Web.UCAN.Signature.RS256.Types                as RS256

import           Web.UCAN.Capabilities.Class
import qualified Web.UCAN.Internal.Base64.URL                  as B64.URL
import           Web.UCAN.Internal.Orphanage.Ed25519.SecretKey ()
import qualified Web.UCAN.Internal.RSA2048.Pair.Types          as RSA2048
import           Web.UCAN.Internal.Time
import qualified Web.UCAN.Internal.UTF8                        as UTF8


-- Reexports

import           Web.UCAN.Capabilities.Types
import           Web.UCAN.Claims.Types
import           Web.UCAN.Header.Types
import           Web.UCAN.Nonce.Types
import           Web.UCAN.RawContent
import           Web.UCAN.Witness.Types


-- | An RFC 7519 extended with support for Ed25519 keys,
--     and some specifics (claims, etc) for Fission's use case
data UCAN fct res abl = UCAN
  { header     :: Header
  , claims     :: Claims fct res abl
  , signedData :: RawContent
  , signature  :: Signature.Signature
  } deriving (Show, Eq)


instance (Eq fct, Eq res, Eq abl) => Ord (UCAN fct res abl) where
  compare x y =
    textDisplay x `compare` textDisplay y


instance Display (UCAN fct res abl) where
  textDisplay UCAN{..} =
    textDisplay signedData <> "." <> textDisplay signature


instance
  ( Arbitrary fct
  , Arbitrary res
  , Arbitrary abl
  , ToJSON fct
  , IsResource res
  , IsAbility abl
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

    claims <- arbitraryClaims arbitraryWitnesses (DID.Key pk)

    return case sk of
      Left rsaSK ->
        case Unsafe.unsafePerformIO $ signRS256 rsaSK claims of
          Left err   -> error $ "Unable to sign UCAN: " <> show err
          Right ucan -> ucan

      Right edSK ->
        signEd25519 edSK claims

    where
      arbitraryWitnesses :: Gen [Witness]
      arbitraryWitnesses = map (Nested . textDisplay) <$> sized arbitraryUCAN

      arbitraryUCAN :: Int -> Gen [UCAN fct res abl]
      arbitraryUCAN size = do
        numChildren <- choose (0, size)
        distributedSizes <- distributeRands numChildren size
        mapM (\childSize -> resize childSize arbitrary) distributedSizes

      distributeRands :: Int -> Int -> Gen [Int]
      distributeRands 0 _ = return []
      distributeRands numChildren available = do
        chosen <- choose (0, available)
        rest <- distributeRands (numChildren - 1) (available - chosen)
        return (chosen:rest)


instance
  ( ToJSON fct
  , IsResource res
  , IsAbility abl
  ) => ToJSON (UCAN fct res abl) where
  toJSON UCAN {..} = String $ content <> "." <> textDisplay signature
    where
      content = decodeUtf8Lenient $ encodeB64 header <> "." <> encodeB64 claims

      encodeB64 jsonable =
        jsonable
          & encode
          & Lazy.toStrict
          & UTF8.stripQuotesBS
          & BS.B64.URL.encodeBase64'
          & UTF8.stripPadding

instance
  ( FromJSON fct
  , IsResource res
  , IsAbility abl
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
  , IsResource res
  , IsAbility abl
  ) => Servant.ToHttpApiData (UCAN fct res abl) where
  toUrlPiece ucan =
    ucan
      & encode
      & Lazy.toStrict
      & decodeUtf8Lenient
      & UTF8.stripQuotes


-----------------------
-- Signature Helpers --
-----------------------


signEd25519 ::
  ( ToJSON fct
  , IsResource res
  , IsAbility abl
  )
  => Ed25519.SecretKey
  -> Claims fct res abl
  -> UCAN fct res abl
signEd25519 sk claims = UCAN{..}
  where
    header     = Header { typ = Typ.JWT, alg = Algorithm.Ed25519, ucv = ucanVersion }
    raw        = B64.URL.encodeJWT header claims
    signedData = RawContent raw
    signature  = Signature.Ed25519 $ Key.signWith sk $ encodeUtf8 raw

signRS256 ::
  ( MonadRandom m
  , ToJSON fct
  , IsResource res
  , IsAbility abl
  )
  => RSA.PrivateKey
  -> Claims fct res abl
  -> m (Either RSA.Error (UCAN fct res abl))
signRS256 sk claims = do
  let raw = B64.URL.encodeJWT header claims
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
  ( FromJSON   fct
  , IsResource rsc
  , IsAbility  abl
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
      res <- maybe (fail $ "Couldn't parse UCAN v0.3 resource: " <> Text.unpack rsc) pure $ parseResourceV_0_3 rsc
      abl <- maybe (fail $ "Couldn't parse UCAN v0.3 ability: " <> Text.unpack pot) pure $ parseAbilityV_0_3 pot
      return [CapResource res (Ability abl)]

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
