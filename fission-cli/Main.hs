module Main (main) where

import RIO

import Fission.CLI

main :: IO ()
main = cli commands >>= snd
