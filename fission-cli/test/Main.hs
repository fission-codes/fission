module Main (main) where

import qualified Fission.Test.CLI         as CLI
import           Fission.Test.CLI.Prelude

main :: IO ()
main = do
  spec <- testSpecs $ parallel CLI.spec
  defaultMain $ testGroup "Tests" spec
