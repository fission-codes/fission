module Main (main) where

import           Fission.Prelude

import           Fission.CLI

main :: IO ()
main =
  cli >>= \case
    Left  _ -> exitFailure
    Right _ -> exitSuccess
