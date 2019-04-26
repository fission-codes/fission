{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude

import System.FilePath.Glob (glob)
import Test.DocTest         (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doctest
