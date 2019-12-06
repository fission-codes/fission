{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import           RIO
import qualified RIO.Partial as Part
import           RIO.Process

import Data.List   (genericLength)
import Data.Maybe  (catMaybes)
import System.Exit (exitFailure, exitSuccess)
import Text.Regex  (matchRegex, mkRegex)
import Fission.App (runApp)

main :: IO ()
main = runApp do
  output <- proc "cabal" ["new-haddock"] readProcessStdout_

  if average (match $ show output) >= expected
    then liftIO exitSuccess
    else do
      logWarn $ displayShow output
      liftIO exitFailure

match :: String -> [Int]
match = fmap Part.read
      . concat
      . catMaybes
      . fmap (matchRegex $ mkRegex "^ *([0-9]*)% ")
      . lines

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 0
