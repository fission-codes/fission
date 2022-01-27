module Main (main) where

import           Test.Web.UCAN.Prelude

import qualified Test.Web.UCAN         as UCAN


main :: IO ()
main = do
  spec <- testSpecs $ parallel UCAN.spec
  defaultMain $ testGroup "Tests" spec
