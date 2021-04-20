module Main (main) where

import           Fission.Test.CLI.Prelude

import qualified Fission.Test.CLI         as CLI

main :: IO ()
main = do
  spec <- testSpecs $ parallel CLI.spec
  defaultMain $ testGroup "Tests" spec
