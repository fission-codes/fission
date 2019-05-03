{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import RIO

import System.FilePath.Glob (glob)
import Test.DocTest         (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doctest
