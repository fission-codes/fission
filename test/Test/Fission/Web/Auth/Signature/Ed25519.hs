module Test.Fission.Web.Auth.Signature.Ed25519 (tests) where

import RIO.Char (ord)

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified RIO.ByteString.Lazy        as Lazy

import           Fission.Web.Auth.JWT

import           Test.Fission.Prelude
import qualified System.IO.Unsafe as Unsafe

import Fission.User.DID

import Fission.Key.Asymmetric.Algorithm.Types
import Fission.Web.Auth.JWT.Signature.Types
import qualified Fission.Web.Auth.JWT.Signature.RS256.Types as RS256

import Fission.Key

import Data.Time.Clock

import Fission.Web.Auth.JWT.Signature.Ed25519 as Ed25519
import qualified Crypto.PubKey.Ed25519 as Ed25519

tests :: SpecWith ()
tests =
  describe "Fission.Web.Auth.Signature.Ed25519" do
    describe "signature verification" do
      itsProp' "foo " \(jwt@JWT {..}, sk :: Ed25519.SecretKey) ->
        let
          sig' = Ed25519.sign header claims
          pk = PublicKey
          checkEd25519Signature $ Ed25519.sign header claims
