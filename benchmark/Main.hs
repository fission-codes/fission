{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude
import Criterion
import Criterion.Main

import Lib (inc)

main :: IO ()
main = defaultMain [bench "inc 41" (whnf inc (41 :: Int))]
