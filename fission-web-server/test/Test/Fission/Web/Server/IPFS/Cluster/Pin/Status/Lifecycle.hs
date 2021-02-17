module Test.Fission.Web.Server.IPFS.Cluster.Pin.Status.Lifecycle where

import qualified RIO.ByteString.Lazy                                        as Lazy

import           Fission.Web.Server.IPFS.Cluster.Pin.Status.Lifecycle.Types

import           Test.Fission.Prelude

tests :: IO TestTree
tests =
  testSpec "Cluster pin lifecycle" do
    describe "Ord instance" do
      it "puts Queued before Pinning" do
        Queued `shouldSatisfy` (< Pinning)

      it "puts Pinning before PinComplete" do
        Pinning `shouldSatisfy` (< PinComplete)
