module Main where

import RIO

import Fission
import Fission.Web

main :: IO ()
main = do
  -- port <- read <$> getLine :: IO Word
  -- startAtPort app port
  runRIO defaultEnv $ startApp app
