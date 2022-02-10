module Main (main) where

import           Test.Web.UCAN.Prelude

import qualified Test.Web.DID  as DID
import qualified Test.Web.UCAN as UCAN


main :: IO ()
main = do
  spec <- testSpecs $ parallel $ describe "Web" do
    UCAN.spec
    DID.spec
  defaultMain $ testGroup "Tests" spec
