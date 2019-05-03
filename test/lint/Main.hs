{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import RIO

import Language.Haskell.HLint (hlint)
import System.Exit            (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "benchmark"
    , "server"
    , "library"
    , "test-suite"
    ]

main :: IO ()
main = do
  hints <- hlint arguments
  if null hints
    then exitSuccess
    else exitFailure
