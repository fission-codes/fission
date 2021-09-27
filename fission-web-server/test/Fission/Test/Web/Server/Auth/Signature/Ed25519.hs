module Fission.Test.Web.Server.Auth.Signature.Ed25519 (spec) where

import qualified Crypto.PubKey.Ed25519                  as Ed25519

import           Fission.Web.Auth.Token.JWT
import           Fission.Web.Auth.Token.JWT.Validation

import           Fission.Key.Asymmetric.Algorithm.Types
import qualified Fission.Web.Auth.Token.JWT.RawContent  as JWT

import qualified Fission.Key                            as Key
import           Fission.User.DID                       as DID

import qualified Fission.Internal.Base64.URL            as B64.URL

import           Fission.Test.Web.Server.Prelude

spec :: Spec
spec =
  describe "Fission.Web.Auth.Signature.Ed25519" do
    describe "signature verification" do
      itsProp' "verifies" \(jwt@JWT {..}, sk) ->
        let
          pk         = Ed25519.toPublic sk
          header'    = header { alg = Ed25519 }
          claims'    = claims { sender = did }
          sig'       = signEd25519 header' claims' sk
          rawContent = JWT.RawContent $ B64.URL.encodeJWT header' claims'
          did        = DID.Key $ Key.Ed25519PublicKey pk

          jwt' = jwt
            { header = header'
            , claims = claims'
            , sig    = sig'
            }

        in
          checkEd25519Signature rawContent jwt' `shouldBe` Right jwt'
