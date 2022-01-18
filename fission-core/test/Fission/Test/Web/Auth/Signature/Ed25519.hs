module Fission.Test.Web.Auth.Signature.Ed25519 (spec) where

import qualified Crypto.PubKey.Ed25519                            as Ed25519
import           Fission.Web.Auth.Token.JWT.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Web.JWT.Types
import           Web.JWT.Validation

import           Crypto.Key.Asymmetric.Algorithm.Types
import qualified Web.JWT.RawContent                               as JWT

import qualified Crypto.Key.Asymmetric                            as Key
import           Web.DID.Types

import qualified Ucan.Internal.Base64.URL                         as B64.URL

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Fission.Web.Auth.Signature.Ed25519" do
    describe "signature verification" do
      itsProp' "verifies" \(jwt@JWT {..} :: JWT Fact (Scope Resource), sk) ->
        let
          pk         = Ed25519.toPublic sk
          header'    = header { alg = Ed25519 }
          claims'    = claims { sender = did }
          sig'       = signEd25519 header' claims' sk
          rawContent = JWT.RawContent $ B64.URL.encodeJWT header' claims'

          did = DID
            { publicKey = Key.Ed25519PublicKey pk
            , method    = Key
            }

          jwt' = jwt
            { header = header'
            , claims = claims'
            , sig    = sig'
            }

        in
          checkEd25519Signature rawContent jwt' `shouldBe` Right jwt'
