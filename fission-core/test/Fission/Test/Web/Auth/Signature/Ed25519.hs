module Fission.Test.Web.Auth.Signature.Ed25519 (spec) where

import qualified Fission.Web.Auth.Token.UCAN.Types     as Fission

import qualified Crypto.Key.Asymmetric                 as Key
import           Crypto.Key.Asymmetric.Algorithm.Types
import qualified Crypto.PubKey.Ed25519                 as Ed25519

import           Web.DID.Types
import qualified Web.UCAN.Internal.Base64.URL          as B64.URL
import qualified Web.UCAN.RawContent                   as UCAN
import           Web.UCAN.Types
import           Web.UCAN.Validation

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Fission.Web.Auth.Signature.Ed25519" do
    describe "signature verification" do
      itsProp' "verifies" \(ucan@UCAN {..} :: Fission.UCAN, sk) ->
        let
          pk         = Ed25519.toPublic sk
          header'    = header { alg = Ed25519 }
          claims'    = claims { sender = did }
          sig'       = signEd25519 header' claims' sk
          rawContent = UCAN.RawContent $ B64.URL.encodeJWT header' claims'

          did = DID
            { publicKey = Key.Ed25519PublicKey pk
            , method    = Key
            }

          ucan' = ucan
            { header = header'
            , claims = claims'
            , sig    = sig'
            }

        in
          checkEd25519Signature rawContent ucan' `shouldBe` Right ucan'
