module Test.Fission.Web.Server.IPFS.Cluster.Pin.Global.Status (tests) where

import qualified RIO.ByteString.Lazy                               as Lazy

import           Fission.Web.Server.IPFS.Cluster.Pin.Global.Status

import           Test.Fission.Prelude

tests :: IO TestTree
tests =
  testSpec "Cluster Status" do
    describe "Real world example" do
      it "Deserializes" do
        let
          realWorld :: Lazy.ByteString
          realWorld = "{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer_map\":{\"QmZ55vfp7dW4pUBctho1Di3xB3sdwtjvqtwZ5NtfWBeMiL\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmZ55vfp7dW4pUBctho1Di3xB3sdwtjvqtwZ5NtfWBeMiL\",\"peername\":\"production-ipfs-cluster-us-east-1-node1\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.293178714Z\",\"error\":\"\"},\"QmZD3iWoPcCCuC7TzTRHAdwrv47mnkk1Tj3BaivEnkm5DG\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmZD3iWoPcCCuC7TzTRHAdwrv47mnkk1Tj3BaivEnkm5DG\",\"peername\":\"production-ipfs-cluster-us-east-1-node0\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.292175673Z\",\"error\":\"\"},\"QmbzBUTTdsWSYf3Xt4NCryDKFMcKPcptPePkPwbqQvHk7j\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmbzBUTTdsWSYf3Xt4NCryDKFMcKPcptPePkPwbqQvHk7j\",\"peername\":\"production-ipfs-cluster-eu-north-1-node1\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.352443413Z\",\"error\":\"\"},\"Qmdb63TsHtV9m1sJfMui7ZLj92ovBWCu47k4Qyp16AMWV8\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"Qmdb63TsHtV9m1sJfMui7ZLj92ovBWCu47k4Qyp16AMWV8\",\"peername\":\"production-ipfs-cluster-us-east-1-node2\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.292930581Z\",\"error\":\"\"},\"QmeY2hDea6NLfupt5gkbUnvifBy68aZ6wSTUTmpufdFtbh\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmeY2hDea6NLfupt5gkbUnvifBy68aZ6wSTUTmpufdFtbh\",\"peername\":\"production-ipfs-cluster-eu-north-1-node0\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.344630765Z\",\"error\":\"\"}}}"

          result :: Either String GlobalPinStatus
          result = eitherDecode realWorld

        result `shouldSatisfy` isRight

      describe "one error" do
        let
          realWorld :: Lazy.ByteString
          realWorld = "{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer_map\":{\"QmZ55vfp7dW4pUBctho1Di3xB3sdwtjvqtwZ5NtfWBeMiL\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmZ55vfp7dW4pUBctho1Di3xB3sdwtjvqtwZ5NtfWBeMiL\",\"peername\":\"production-ipfs-cluster-us-east-1-node1\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.293178714Z\",\"error\":\"\"},\"QmZD3iWoPcCCuC7TzTRHAdwrv47mnkk1Tj3BaivEnkm5DG\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmZD3iWoPcCCuC7TzTRHAdwrv47mnkk1Tj3BaivEnkm5DG\",\"peername\":\"production-ipfs-cluster-us-east-1-node0\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.292175673Z\",\"error\":\"\"},\"QmbzBUTTdsWSYf3Xt4NCryDKFMcKPcptPePkPwbqQvHk7j\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmbzBUTTdsWSYf3Xt4NCryDKFMcKPcptPePkPwbqQvHk7j\",\"peername\":\"production-ipfs-cluster-eu-north-1-node1\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.352443413Z\",\"error\":\"\"},\"Qmdb63TsHtV9m1sJfMui7ZLj92ovBWCu47k4Qyp16AMWV8\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"Qmdb63TsHtV9m1sJfMui7ZLj92ovBWCu47k4Qyp16AMWV8\",\"peername\":\"production-ipfs-cluster-us-east-1-node2\",\"status\":\"unqueued\",\"timestamp\":\"2021-02-16T22:20:01.292930581Z\",\"error\":\"problem\"},\"QmeY2hDea6NLfupt5gkbUnvifBy68aZ6wSTUTmpufdFtbh\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmeY2hDea6NLfupt5gkbUnvifBy68aZ6wSTUTmpufdFtbh\",\"peername\":\"production-ipfs-cluster-eu-north-1-node0\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.344630765Z\",\"error\":\"\"}}}"

          result :: Either String GlobalPinStatus
          result = eitherDecode realWorld

        it "Deserializes" do
          result `shouldSatisfy` isRight

        it "reports the failure" do
          fmap progress result `shouldBe` Right (Failed $ FailedWith "")
