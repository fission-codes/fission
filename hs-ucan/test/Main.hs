module Main (main) where

import           Web.UCAN.Test.Prelude

import qualified Web.UCAN.Test         as UCAN


main :: IO ()
main = do
  spec <- testSpecs $ parallel UCAN.spec
  defaultMain $ testGroup "Tests" spec
