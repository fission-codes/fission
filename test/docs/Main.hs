module Main (main) where

import           RIO
import qualified RIO.ByteString as BS
import qualified RIO.Partial    as Part
import qualified RIO.Process    as Proc

import Data.List   (genericLength)
import Data.Maybe  (catMaybes)
import System.Exit (exitFailure, exitSuccess)

-- TODO move to typed-process
-- import System.Process (readProcess)
import Text.Regex (matchRegex, mkRegex)

main :: IO ()
main = do
    output <- readProcessStdout_ $ proc "stack" ["haddock"]

    if average (match output) >= expected
      then exitSuccess
      else do
        traceDisplayIO $ displayShow output
        exitFailure

match :: String -> [Int]
match = fmap Part.read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 90
