module Main (main) where

import           Fission.Prelude

import qualified Fission.Web.Server.Internal.Production as Production

main :: IO ()
main = Production.runInProd Nothing Production.start
