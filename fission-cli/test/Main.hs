module Main (main) where

import qualified Fission.Test.CLI                     as CLI
import qualified Fission.Test.CLI.Handler.App.Publish as App.Publish
import           Fission.Test.CLI.Prelude

main :: IO ()
main = do
  specs <- testSpecs $ parallel do
    CLI.spec
    App.Publish.spec

  defaultMain $ testGroup "Tests" specs
