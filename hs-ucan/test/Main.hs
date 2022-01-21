module Main (main) where

import           Test.Prelude

import qualified Test.Web.UCAN         as UCAN
import qualified Test.Web.DID as DID


main :: IO ()
main = do
  spec <- testSpecs $ parallel $ describe "Web" do
    UCAN.spec
    DID.spec
  defaultMain $ testGroup "Tests" spec
