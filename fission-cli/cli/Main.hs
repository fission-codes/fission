module Main (main) where

import           Fission.Prelude

import           GHC.IO.Encoding as Encoding

import           Fission.CLI

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8
  cli >>= \case
    Left  _ -> exitFailure
    Right _ -> exitSuccess
