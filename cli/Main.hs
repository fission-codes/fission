{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import RIO

import qualified Network.Wai.Cli as CLI
import           System.Envy

import           Fission.Config
import qualified Fission.Web    as Web

main :: IO ()
main = CLI.defWaiMain =<< return (Web.app (defConfig :: Config))
