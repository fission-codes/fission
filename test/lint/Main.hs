{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import RIO

import Language.Haskell.HLint (hlint)
import System.Exit            (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "benchmark"
    , "app"
    , "src"
    , "test/testsuite"
    ]

main :: IO ()
main = do
  hints <- hlint arguments
  if null hints
    then exitSuccess
    else exitFailure
