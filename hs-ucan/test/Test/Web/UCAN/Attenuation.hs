{-# LANGUAGE DerivingVia #-}
module Test.Web.UCAN.Attenuation (spec) where

import qualified Data.Aeson.Types                      as JSON

import qualified RIO.Set                               as Set

import           Web.UCAN.Capabilities
import           Web.UCAN.Types

import           Test.Prelude

import qualified RIO.List                              as List
import           Test.Web.UCAN.Example
import           Test.Web.UCAN.Orphanage.DummyResolver ()
import           Web.DID.Types
import           Web.UCAN.Witness


spec :: Spec
spec =
  describe "Attenuation" do
    describe "capabilities" do
      itsProp' "produces parenthood proofs on UCANs without proofs" \(ucan :: UCAN () Capability) -> do
        proofs <- capabilities ucan{ claims = (claims ucan){ proofs = [] } }
        let isParenthoodWitness = \case
              Right ProofParenthood{} -> True
              _                       -> False
        all isParenthoodWitness proofs `shouldBe` True

      itsProp' "produces only valid proofs" \(ucan :: UCAN () Capability) -> do
        proofs <- capabilities ucan
        all
          (\case
            Right proof -> checkProof ucan proof
            Left _      -> True
          ) proofs `shouldBe` True

      describe "fixtures" do
        fixtures & foldMapM \(idx, encodedUcan, expectedCapabilities) ->
          it ("works with fixture #" <> show idx) do
            let ucan = parseFixture @(UCAN JSON.Value EmailCapability) $ JSON.String encodedUcan
            results <- capabilities ucan
            let
              caps = results & concatMap \case
                Left _  -> []
                Right v -> [v]

              actualCapabilities =
                map (\cap -> (capability cap, originator cap)) caps

            Set.fromList actualCapabilities `shouldBe`
              Set.fromList expectedCapabilities



newtype EmailCapability
  = EmailCapability Text deriving (Show, Eq, Ord)
  deriving DelegationSemantics via (EqualCanDelegate EmailCapability)

instance FromJSON EmailCapability where
  parseJSON = withObject "UCAN.EmailCapability" \obj -> do
    email         <- obj .: "email"
    (cap :: Text) <- obj .: "cap"
    unless (cap == "SEND") $ fail $ "Failed to parse email capability. Unknown potency '" <> show cap <> "'."
    return $ EmailCapability email


fixtures :: [(Natural, Text, [(EmailCapability, DID)])]
fixtures =
  [ ( 0
    , "eyJhbGciOiJFZERTQSIsInR5cCI6IkpXVCIsInVjdiI6IjAuNy4wIn0.eyJhdWQiOiJkaWQ6a2V5Ono2TWtrODliQzNKclZxS2llNzFZRWNjNU0xU01WeHVDZ054NnpMWjhTWUpzeEFMaSIsImF0dCI6W3siZW1haWwiOiJhbGljZUBlbWFpbC5jb20iLCJjYXAiOiJTRU5EIn0seyJlbWFpbCI6ImJvYkBlbWFpbC5jb20iLCJjYXAiOiJTRU5EIn1dLCJleHAiOjE2NDU0NDA3MzEsImlzcyI6ImRpZDprZXk6ejZNa3RhZlpUUkVqSmt2VjVtZkp4Y0xwTkJvVlB3RExoVHVNZzluZzdkWTR6TUFMIiwicHJmIjpbImV5SmhiR2NpT2lKRlpFUlRRU0lzSW5SNWNDSTZJa3BYVkNJc0luVmpkaUk2SWpBdU55NHdJbjAuZXlKaGRXUWlPaUprYVdRNmEyVjVPbm8yVFd0MFlXWmFWRkpGYWtwcmRsWTFiV1pLZUdOTWNFNUNiMVpRZDBSTWFGUjFUV2M1Ym1jM1pGazBlazFCVENJc0ltRjBkQ0k2VzNzaVpXMWhhV3dpT2lKaGJHbGpaVUJsYldGcGJDNWpiMjBpTENKallYQWlPaUpUUlU1RUluMWRMQ0psZUhBaU9qRTJORFUwTkRBM016RXNJbWx6Y3lJNkltUnBaRHByWlhrNmVqWk5hMnM0T1dKRE0wcHlWbkZMYVdVM01WbEZZMk0xVFRGVFRWWjRkVU5uVG5nMmVreGFPRk5aU25ONFFVeHBJaXdpY0hKbUlqcGJYWDAuc0c5MmlVQ29kMmsyX2k2UXdreFl6NlF6SHF6eVNuay1teXVtWVd5eHlpY2N3TVpDMGVQbDBla3lRUEw3X3ZpLW1JZENGMExuMHYtSjR1OXcwMlZ1QVEiLCJleUpoYkdjaU9pSkZaRVJUUVNJc0luUjVjQ0k2SWtwWFZDSXNJblZqZGlJNklqQXVOeTR3SW4wLmV5SmhkV1FpT2lKa2FXUTZhMlY1T25vMlRXdDBZV1phVkZKRmFrcHJkbFkxYldaS2VHTk1jRTVDYjFaUWQwUk1hRlIxVFdjNWJtYzNaRmswZWsxQlRDSXNJbUYwZENJNlczc2laVzFoYVd3aU9pSmliMkpBWlcxaGFXd3VZMjl0SWl3aVkyRndJam9pVTBWT1JDSjlYU3dpWlhod0lqb3hOalExTkRRd056TXhMQ0pwYzNNaU9pSmthV1E2YTJWNU9ubzJUV3RtWmtSYVEydERWRmR5WldjNE9EWTRaa2N4UmtkR2IyZGpTbW8xV0RaUVdUa3pjRkJqVjBSdU9XSnZZaUlzSW5CeVppSTZXMTE5LkduSFZQSUVJSUIteVFhSDl1U2p2VjNKNm5xc2Zoa3dJZFpaZU1VX0RUd04wRElDSFpLaEpzXzltTUdIS3RmNWJlT0lja21vdFQwc1l4cUZvajB0NUNBIl19.v5S8vIYqbf0RnYdqBi61-R-SjtEcpeSanB7UYw479sD7IKztTlsZRYAqxXBZEqIDV0Tiq-FzOroVj_BrUYmoAA"
    , [ ( EmailCapability "alice@email.com"
        , aliceDID
        )
      , ( EmailCapability "bob@email.com"
        , bobDID
        )
      , ( EmailCapability "alice@email.com"
        , malloryDID
        )
      , ( EmailCapability "bob@email.com"
        , malloryDID
        )
      ]
    )
  ]


aliceDID, bobDID, malloryDID :: DID
aliceDID   = parseFixture $ JSON.String "did:key:z6Mkk89bC3JrVqKie71YEcc5M1SMVxuCgNx6zLZ8SYJsxALi"
bobDID     = parseFixture $ JSON.String "did:key:z6MkffDZCkCTWreg8868fG1FGFogcJj5X6PY93pPcWDn9bob"
malloryDID = parseFixture $ JSON.String "did:key:z6MktafZTREjJkvV5mfJxcLpNBoVPwDLhTuMg9ng7dY4zMAL"

parseFixture :: FromJSON a => JSON.Value -> a
parseFixture val = case JSON.fromJSON val of
  JSON.Success a -> a
  JSON.Error str -> error $ "Failed to parse fixture: " <> str

