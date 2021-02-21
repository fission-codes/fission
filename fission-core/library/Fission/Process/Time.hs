module Fission.Process.Time
  ( sleepThread
  , module Fission.Process.Time.Error
  )  where

import           Fission.Prelude

import           Fission.Time

import           Fission.Process.Time.Error

sleepThread :: forall m prefix s .
  ( MonadIO      m
  , Integral    (prefix s)
  , Num         (prefix Double)
  , FromPrefixed prefix Double
  )
  => Seconds prefix s
  -> m ()
sleepThread (Seconds s) = threadDelay $ truncate us
  where
    us :: Double
    Micro us = convert asDouble

    asDouble :: prefix Double
    asDouble = fromIntegral s
