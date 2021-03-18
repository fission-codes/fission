module Main (main) where

import           Fission.Test.Web.Server.Prelude

import qualified Fission.Test.Web.Server         as Server

main :: IO ()
main = do
  spec <- testSpecs Server.spec
  defaultMain $ testGroup "Tests" spec
