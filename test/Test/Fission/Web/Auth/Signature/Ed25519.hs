module Test.Fission.Web.Auth.Signature.Ed25519 (tests) where

import RIO.Char (ord)

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified RIO.ByteString.Lazy        as Lazy

import           Fission.Web.Auth.JWT
import qualified Fission.Key as Key

import           Test.Fission.Prelude
import qualified System.IO.Unsafe as Unsafe

import Fission.User.DID
import qualified Crypto.PubKey.Ed25519    as Crypto.Ed25519

import qualified Fission.Web.Auth.JWT.Signature.Ed25519 as Ed25519X

import Fission.Key.Asymmetric.Algorithm.Types as Alg
import Fission.Web.Auth.JWT.Signature.Types
import qualified Fission.Web.Auth.JWT.Signature.RS256.Types as RS256

import Fission.Key
import Fission.Key.Store as Store
import qualified Crypto.PubKey.Ed25519 as Ed

import Data.Time.Clock

import Fission.Web.Auth.JWT.Signature.Ed25519 as Ed25519
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.PubKey.Ed25519 as Crypto.Ed25519

import qualified Fission.Internal.Crypto as Crypto
import qualified Fission.Internal.UTF8 as UTF8

import           Fission.User.DID.Method.Types

import qualified Fission.Web.Auth.JWT.Signature.Types as Signature

tests :: SpecWith ()
tests =
  describe "Fission.Web.Auth.Signature.Ed25519" do
    describe "signature verification" do
      itsProp' "foo " \(jwt@JWT {..}, sk :: Ed25519.SecretKey) ->
        let
          header' = header { alg = Alg.Ed25519 }
          pk = Ed.toPublic sk
          did = DID
            { publicKey = Key.Public $ decodeUtf8Lenient $ Crypto.toBase64 $ Crypto.unpack $ pk
            , algorithm = Alg.Ed25519
            , method = Key
            }

          claims' = claims
            { iss = did
            }
          sig@(Signature.Ed25519 sig') = Ed25519X.sign header' claims' sk
          jwt' = jwt
            { header = header'
            , claims = claims'
            , sig = sig
            }
          content =
                -- UTF8.stripOptionalSuffixBS "=" $
                -- UTF8.stripOptionalSuffixBS "=" $
                -- Crypto.toBase64 $
                Lazy.toStrict $ encode header' <> "." <> encode claims'

          -- hw = Crypto.toBase64 ("hello world" :: ByteString)
        in
          (checkEd25519Signature jwt') `shouldBe` Right jwt'
          -- show (Crypto.Ed25519.verify pk content sig') `shouldBe` ""

          -- show (Crypto.Ed25519.verify pk content sig') `shouldBe` "True"
