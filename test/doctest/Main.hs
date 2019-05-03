{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO

import System.FilePath.Glob (glob)
import Test.DocTest         (doctest)

main :: IO ()
main = doctest =<< glob "src/**/*.hs"
