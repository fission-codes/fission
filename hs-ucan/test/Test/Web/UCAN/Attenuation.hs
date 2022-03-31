{-# LANGUAGE DerivingVia #-}
module Test.Web.UCAN.Attenuation (spec) where

import           Control.Monad.Time
import qualified Crypto.Error
import           Crypto.Key.Asymmetric.Public.Types
import qualified Crypto.PubKey.Ed25519                 as Ed25519
import qualified Data.Aeson.Types                      as JSON
import qualified Data.ByteArray                        as BA
import qualified Data.ByteString.Base64                as BS64
import           Data.Coerce
import           Data.Monoid
import qualified RIO.ByteString                        as BS
import qualified RIO.Set                               as Set
import qualified RIO.Text                              as Text

import           Test.Web.UCAN.Prelude

import qualified RIO.List                              as List
import           RIO.Time
import qualified Test.Web.UCAN.DelegationSemantics     as DelegationSemantics
import qualified Test.Web.UCAN.Example                 as Ex
import           Test.Web.UCAN.Orphanage.DummyResolver ()
import           Web.DID.Types
import qualified Web.DID.Types                         as DID
import           Web.UCAN.Capabilities
import           Web.UCAN.Types
import           Web.UCAN.Witness


spec :: Spec
spec =
  describe "Attenuation" do
    describe "capabilities" do
      itsProp' "produces parenthood proofs on UCANs without proofs" \(ucan :: UCAN () Ex.Resource Ex.Ability) -> do
        proofs <- capabilities ucan{ claims = (claims ucan){ proofs = [] } }
        let isParenthoodWitness = \case
              Right (ProofAuthorization _ _ _ (ProofByDelegation _)) -> False
              _                                                      -> True
        all isParenthoodWitness proofs `shouldBe` True

      itsProp' "produces only valid proofs" \(ucan :: UCAN () Ex.Resource Ex.Ability) -> do
        proofs <- capabilities ucan
        all
          (\case
            Right proof -> checkProof proof
            Left _      -> True
          ) proofs `shouldBe` True

      describe "fixtures" do
        fixtures & foldMapM \(idx, encodedUcan, expectedCapabilities) ->
          it ("works with ts-ucan-generated fixture #" <> show idx) do
            let ucan = parseFixture @(UCAN JSON.Value EmailResource DummyAbility) $ JSON.String encodedUcan
            actualCapabilities <- capsWithOriginators <$> capabilities ucan
            Set.fromList actualCapabilities `shouldBe`
              Set.fromList expectedCapabilities

        it "works with partial-order delgation semantics" do
          now <- currentTime
          let
            tomorrow = addUTCTime nominalDay now

            leafUcan = signEd25519 @JSON.Value aliceKey Claims
              { sender = aliceDID
              , receiver = bobDID
              , attenuation =
                [ CapResource
                    (PathCapability ["public", "test"])
                    (Ability AnyAbility)
                , CapResource
                    (PathCapability ["public", "Apps"])
                    (Ability AnyAbility)
                ]
              , proofs = []
              , facts = []
              , expiration = tomorrow
              , notBefore = Nothing
              , nonce = Nothing
              }

            ucan = signEd25519 @JSON.Value bobKey Claims
              { sender = bobDID
              , receiver = malloryDID
              , attenuation =
                [ CapResource
                    (PathCapability ["public", "test", "file.txt"])
                    (Ability AnyAbility)
                , CapResource
                    (PathCapability ["public", "abc"])
                    (Ability AnyAbility)
                ]
              , proofs = [ Nested $ textDisplay leafUcan ]
              , facts = []
              , expiration = tomorrow
              , notBefore = Nothing
              , nonce = Nothing
              }

            expectedCapabilities =
              [ ( (PathCapability ["public", "test", "file.txt"], Ability AnyAbility)
                , aliceDID
                )
              , ( (PathCapability ["public", "test", "file.txt"], Ability AnyAbility)
                , bobDID
                )
              , ( (PathCapability ["public", "abc"], Ability AnyAbility)
                , bobDID
                )
              ]

          actualCapabilities <- capsWithOriginators <$> capabilities ucan

          Set.fromList expectedCapabilities `shouldBe`
            Set.fromList actualCapabilities

      describe "PathCapability" do
        DelegationSemantics.itHasPartialOrderProperties @PathCapability

        it "public/ can delegate public/test" do
          (PathCapability ["public"] `canDelegate` PathCapability ["public", "test"])
            `shouldBe` True

        it "public/ can not delegate private/" do
          (PathCapability ["public"] `canDelegate` PathCapability ["private"])
            `shouldBe` False

        it "public/test can not delegate public/" do
          (PathCapability ["public", "test"] `canDelegate` PathCapability ["public"])
            `shouldBe` False


newtype EmailResource
  = EmailResource Text deriving (Show, Eq, Ord)
  deriving DelegationSemantics via (EqualCanDelegate EmailResource)

data DummyAbility = AnyAbility deriving (Show, Eq, Ord)
  deriving DelegationSemantics via (EqualCanDelegate DummyAbility)

instance FromJSON EmailResource where
  parseJSON = withObject "UCAN.EmailResource" \obj -> do
    email <- obj .: "with"
    return $ EmailResource email

instance Display EmailResource where
  textDisplay (EmailResource email) = email

instance FromJSON DummyAbility where
  parseJSON = withObject "UCAN.DummyAbility" \_ -> do
    return AnyAbility

instance Display DummyAbility where
  textDisplay AnyAbility = "DUMMY_ABILITY"

data PathCapability
  = PathCapability [Text] deriving (Show, Eq, Ord)

instance FromJSON PathCapability where
  parseJSON = withObject "UCAN.PathCapability" \obj -> do
    (path :: Text) <- obj .: "with"
    return $ PathCapability $ Text.split (== '/') path

instance Display PathCapability where
  textDisplay (PathCapability path) = mconcat $ List.intersperse "/" path

instance Arbitrary PathCapability where
  arbitrary = do
    len <- chooseInt (0, 5)
    segments <- sequence (List.replicate len (elements ["public", "private", "test", "Apps", "Docs"]))
    return $ PathCapability segments

instance DelegationSemantics PathCapability where
  (PathCapability parentPath) `canDelegate` (PathCapability childPath) =
    length parentPath <= length childPath
      && (coerce @([All] -> All) mconcat) (zipWith (==) parentPath childPath)


fixtures :: [(Natural, Text, [((EmailResource, Ability DummyAbility), DID)])]
fixtures =
  [ ( 0
    , "eyJhbGciOiJFZERTQSIsInR5cCI6IkpXVCIsInVjdiI6IjAuNy4wIn0.eyJhdWQiOiJkaWQ6a2V5Ono2TWtrODliQzNKclZxS2llNzFZRWNjNU0xU01WeHVDZ054NnpMWjhTWUpzeEFMaSIsImF0dCI6W3siZW1haWwiOiJhbGljZUBlbWFpbC5jb20iLCJjYXAiOiJTRU5EIn0seyJlbWFpbCI6ImJvYkBlbWFpbC5jb20iLCJjYXAiOiJTRU5EIn1dLCJleHAiOjE2NDU0NDA3MzEsImlzcyI6ImRpZDprZXk6ejZNa3RhZlpUUkVqSmt2VjVtZkp4Y0xwTkJvVlB3RExoVHVNZzluZzdkWTR6TUFMIiwicHJmIjpbImV5SmhiR2NpT2lKRlpFUlRRU0lzSW5SNWNDSTZJa3BYVkNJc0luVmpkaUk2SWpBdU55NHdJbjAuZXlKaGRXUWlPaUprYVdRNmEyVjVPbm8yVFd0MFlXWmFWRkpGYWtwcmRsWTFiV1pLZUdOTWNFNUNiMVpRZDBSTWFGUjFUV2M1Ym1jM1pGazBlazFCVENJc0ltRjBkQ0k2VzNzaVpXMWhhV3dpT2lKaGJHbGpaVUJsYldGcGJDNWpiMjBpTENKallYQWlPaUpUUlU1RUluMWRMQ0psZUhBaU9qRTJORFUwTkRBM016RXNJbWx6Y3lJNkltUnBaRHByWlhrNmVqWk5hMnM0T1dKRE0wcHlWbkZMYVdVM01WbEZZMk0xVFRGVFRWWjRkVU5uVG5nMmVreGFPRk5aU25ONFFVeHBJaXdpY0hKbUlqcGJYWDAuc0c5MmlVQ29kMmsyX2k2UXdreFl6NlF6SHF6eVNuay1teXVtWVd5eHlpY2N3TVpDMGVQbDBla3lRUEw3X3ZpLW1JZENGMExuMHYtSjR1OXcwMlZ1QVEiLCJleUpoYkdjaU9pSkZaRVJUUVNJc0luUjVjQ0k2SWtwWFZDSXNJblZqZGlJNklqQXVOeTR3SW4wLmV5SmhkV1FpT2lKa2FXUTZhMlY1T25vMlRXdDBZV1phVkZKRmFrcHJkbFkxYldaS2VHTk1jRTVDYjFaUWQwUk1hRlIxVFdjNWJtYzNaRmswZWsxQlRDSXNJbUYwZENJNlczc2laVzFoYVd3aU9pSmliMkpBWlcxaGFXd3VZMjl0SWl3aVkyRndJam9pVTBWT1JDSjlYU3dpWlhod0lqb3hOalExTkRRd056TXhMQ0pwYzNNaU9pSmthV1E2YTJWNU9ubzJUV3RtWmtSYVEydERWRmR5WldjNE9EWTRaa2N4UmtkR2IyZGpTbW8xV0RaUVdUa3pjRkJqVjBSdU9XSnZZaUlzSW5CeVppSTZXMTE5LkduSFZQSUVJSUIteVFhSDl1U2p2VjNKNm5xc2Zoa3dJZFpaZU1VX0RUd04wRElDSFpLaEpzXzltTUdIS3RmNWJlT0lja21vdFQwc1l4cUZvajB0NUNBIl19.v5S8vIYqbf0RnYdqBi61-R-SjtEcpeSanB7UYw479sD7IKztTlsZRYAqxXBZEqIDV0Tiq-FzOroVj_BrUYmoAA"
    , [ ( (EmailResource "alice@email.com", Ability AnyAbility)
        , aliceDID
        )
      , ( (EmailResource "bob@email.com", Ability AnyAbility)
        , bobDID
        )
      -- We also infer capabilities by parenthood, even though they *could* be delegated.
      -- Either having the capability through ambient authority or delegation suffices.
      , ( (EmailResource "alice@email.com", Ability AnyAbility)
        , malloryDID
        )
      , ( (EmailResource "bob@email.com", Ability AnyAbility)
        , malloryDID
        )
      ]
    )
  , ( 1
    , "eyJhbGciOiJFZERTQSIsInR5cCI6IkpXVCIsInVjdiI6IjAuNy4wIn0.eyJhdWQiOiJkaWQ6a2V5Ono2TWtrODliQzNKclZxS2llNzFZRWNjNU0xU01WeHVDZ054NnpMWjhTWUpzeEFMaSIsImF0dCI6W3siZW1haWwiOiJhbGljZUBlbWFpbC5jb20iLCJjYXAiOiJTRU5EIn1dLCJleHAiOjE2NDU0NTgyMTEsImlzcyI6ImRpZDprZXk6ejZNa3RhZlpUUkVqSmt2VjVtZkp4Y0xwTkJvVlB3RExoVHVNZzluZzdkWTR6TUFMIiwicHJmIjpbImV5SmhiR2NpT2lKRlpFUlRRU0lzSW5SNWNDSTZJa3BYVkNJc0luVmpkaUk2SWpBdU55NHdJbjAuZXlKaGRXUWlPaUprYVdRNmEyVjVPbm8yVFd0MFlXWmFWRkpGYWtwcmRsWTFiV1pLZUdOTWNFNUNiMVpRZDBSTWFGUjFUV2M1Ym1jM1pGazBlazFCVENJc0ltRjBkQ0k2VzNzaVpXMWhhV3dpT2lKaGJHbGpaVUJsYldGcGJDNWpiMjBpTENKallYQWlPaUpUUlU1RUluMWRMQ0psZUhBaU9qRTJORFUwTlRneU1UQXNJbWx6Y3lJNkltUnBaRHByWlhrNmVqWk5hMnM0T1dKRE0wcHlWbkZMYVdVM01WbEZZMk0xVFRGVFRWWjRkVU5uVG5nMmVreGFPRk5aU25ONFFVeHBJaXdpY0hKbUlqcGJYWDAuemdjdTBLV0VLOS1RNERPZk5wdlFyb3lQTkwwX1R2YmJ3SzVTQXVWWTJocEFCaU1LQnRuY1Z0Rm9VSWY0VmdKemRTX0FnYWdNMlB1Z2p3U2xrcnYzREEiLCJleUpoYkdjaU9pSkZaRVJUUVNJc0luUjVjQ0k2SWtwWFZDSXNJblZqZGlJNklqQXVOeTR3SW4wLmV5SmhkV1FpT2lKa2FXUTZhMlY1T25vMlRXdDBZV1phVkZKRmFrcHJkbFkxYldaS2VHTk1jRTVDYjFaUWQwUk1hRlIxVFdjNWJtYzNaRmswZWsxQlRDSXNJbUYwZENJNlczc2laVzFoYVd3aU9pSmhiR2xqWlVCbGJXRnBiQzVqYjIwaUxDSmpZWEFpT2lKVFJVNUVJbjFkTENKbGVIQWlPakUyTkRVME5UZ3lNVEVzSW1semN5STZJbVJwWkRwclpYazZlalpOYTJabVJGcERhME5VVjNKbFp6ZzROamhtUnpGR1IwWnZaMk5LYWpWWU5sQlpPVE53VUdOWFJHNDVZbTlpSWl3aWNISm1JanBiWFgwLi1BVTduZXdkbDNhbUJzQVV5VE5COUZ0cGU3bEEwc1J1dmtTdXNpNXlwVDhjbWxnSlRCWDc2RmNIS2hGUFhrVHRibXpUYVpWUkMzRDdSRThZeHViTERBIl19.c5hWPRZ90N9a_vqoA79PrtH8Ej6U-R73xg5ZKiqLqYkz28gwcv0v_PTLEbke2OU_hBJvMaj9Y9ickxr28xlEDw"
    , [ ( (EmailResource "alice@email.com", Ability AnyAbility)
        , aliceDID
        )
      , ( (EmailResource "alice@email.com", Ability AnyAbility)
        , bobDID
        )
      , ( (EmailResource "alice@email.com", Ability AnyAbility)
        , malloryDID
        )
      ]
    )
  ]


aliceDID, bobDID, malloryDID :: DID
aliceDID   = DID.Key $ Ed25519PublicKey alicePublic -- parseFixture $ JSON.String "did:key:z6Mkk89bC3JrVqKie71YEcc5M1SMVxuCgNx6zLZ8SYJsxALi"
bobDID     = DID.Key $ Ed25519PublicKey bobPublic -- parseFixture $ JSON.String "did:key:z6MkffDZCkCTWreg8868fG1FGFogcJj5X6PY93pPcWDn9bob"
malloryDID = DID.Key $ Ed25519PublicKey malloryPublic -- parseFixture $ JSON.String "did:key:z6MktafZTREjJkvV5mfJxcLpNBoVPwDLhTuMg9ng7dY4zMAL"


aliceKey   , bobKey   , malloryKey    :: Ed25519.SecretKey
alicePublic, bobPublic, malloryPublic :: Ed25519.PublicKey
(aliceKey  , alicePublic)   = expectFixture $ parseNaClEd25519SecretKeyBase64 "U+bzp2GaFQHso587iSFWPSeCzbSfn/CbNHEz7ilKRZ1UQMmMS7qq4UhTzKn3X9Nj/4xgrwa+UqhMOeo4Ki8JUw=="
(bobKey    , bobPublic)     = expectFixture $ parseNaClEd25519SecretKeyBase64 "G4+QCX1b3a45IzQsQd4gFMMe0UB1UOx9bCsh8uOiKLER69eAvVXvc8P2yc4Iig42Bv7JD2zJxhyFALyTKBHipg=="
(malloryKey, malloryPublic) = expectFixture $ parseNaClEd25519SecretKeyBase64 "LR9AL2MYkMARuvmV3MJV8sKvbSOdBtpggFCW8K62oZDR6UViSXdSV/dDcD8S9xVjS61vh62JITx7qmLgfQUSZQ=="


parseFixture :: FromJSON a => JSON.Value -> a
parseFixture val = case JSON.fromJSON val of
  JSON.Success a -> a
  JSON.Error str -> error $ "Failed to parse fixture: " <> str


expectFixture :: Show e => Either e a -> a
expectFixture = \case
  Left e  -> error $ show e
  Right a -> a


parseNaClEd25519SecretKeyBase64 :: ByteString -> Either Text (Ed25519.SecretKey, Ed25519.PublicKey)
parseNaClEd25519SecretKeyBase64 bs = do
  bytes <- BS64.decodeBase64 bs
  () <- if BS.length bytes == 64 then Right () else Left $ "Unexpected length (" <> textDisplay (BS.length bs) <> " bytes). Expected 64 bytes."
  let (secretKeyBytes, publicKeyBytes) = BS.splitAt 32 bytes
  secretKey <- ensureCryptoPassed $ Ed25519.secretKey secretKeyBytes
  let publicKey = Ed25519.toPublic secretKey
  let actualPublicKeyBytes = BS.pack $ BA.unpack publicKey
  () <- if actualPublicKeyBytes == publicKeyBytes then Right () else Left $ "Secret key didn't match expected public key."
  return (secretKey, publicKey)
  where
    ensureCryptoPassed = \case
      Crypto.Error.CryptoPassed a   -> Right a
      Crypto.Error.CryptoFailed err -> Left $ Text.pack $ show err


capsWithOriginators :: [Either a (Proof fct res abl)] -> [((res, Ability abl), DID)]
capsWithOriginators = concatMap \case
  Right proof@(ProofAuthorization res abl _ _) -> [((res, abl), originator proof)]
  _                                            -> []
