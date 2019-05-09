{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import RIO

import Network.Wai.Cli (defWaiMain)
import System.Envy

import qualified Fission.Web as Web

main :: IO ()
main = defWaiMain =<< return (Web.app defConfig)
