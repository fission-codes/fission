{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Security
  ( Secret
  , genSecret
  ) where

import RIO

import qualified Fission.Random as Random

type Secret = Text

genSecret :: Int -> IO (Either UnicodeException Secret)
genSecret amt = return . decodeUtf8' =<< Random.text amt
