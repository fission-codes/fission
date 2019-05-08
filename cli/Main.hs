{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import RIO

import Network.Wai.Cli (defWaiMain)

import qualified Fission.Env as Env
import qualified Fission.Web as Web

main :: IO ()
main = defWaiMain =<< return (Web.app Env.base)
