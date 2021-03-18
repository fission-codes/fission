module Main (main) where

import           Fission.Test.Prelude

import qualified Fission.Test         as Fission

main :: IO ()
main = do
  spec <- testSpecs Fission.spec
  defaultMain $ testGroup "Tests" spec
