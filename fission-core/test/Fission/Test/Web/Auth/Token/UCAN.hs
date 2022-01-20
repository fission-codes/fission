module Fission.Test.Web.Auth.Token.UCAN (spec) where

import qualified Data.Aeson                                  as JSON
import qualified RIO.ByteString.Lazy                         as Lazy
import           Servant.API

import qualified Fission.Internal.UTF8                       as UTF8
import qualified Fission.Web.Auth.Token.UCAN.Types           as Fission

import           Fission.Test.Prelude
import qualified Fission.Test.Web.Auth.Token.UCAN.Validation as Validation

import qualified Fission.Test.Web.Auth.Token.UCAN.Proof      as Proof

spec :: Spec
spec =
  describe "Fission.Web.Auth.Token.UCAN" do
    Proof.spec
    Validation.spec

    describe "Header serialization" do
      itsProp' "text serialization is unquoted JSON" \(ucan :: Fission.UCAN) ->
        ucan
          |> toUrlPiece
          |> UTF8.wrapIn "\""
          |> encodeUtf8
          |> Lazy.fromStrict
          |> shouldBe (JSON.encode ucan)
