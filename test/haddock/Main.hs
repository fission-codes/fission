{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import           RIO
import qualified RIO.ByteString as BS
import qualified RIO.Partial    as Part

import Data.List      (genericLength)
import Data.Maybe     (catMaybes)
import System.Exit    (exitFailure, exitSuccess)
import System.Process (readProcess)
import Text.Regex     (matchRegex, mkRegex)

import qualified Fission.Internal.UTF8 as UTF8

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 90

main :: IO ()
main = do
    output <- readProcess "stack" ["haddock"] ""

    if average (match output) >= expected
      then exitSuccess
      else do
        BS.putStr . UTF8.encode $ displayShow output
        exitFailure

match :: String -> [Int]
match = fmap Part.read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
