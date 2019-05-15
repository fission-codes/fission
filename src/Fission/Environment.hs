{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Environment (Environment (..)) where

import RIO
import RIO.Text

data Environment
  = Test
  | Development
  --  | Staging
  | Production
  deriving (Eq, Show, Read)

instance Display Environment where
  display     = displayShow
  textDisplay = pack . show
