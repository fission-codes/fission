{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.Heroku.UserConfig
  ( UserConfig (..)
  , fissionApiUrl
  ) where

import RIO

import Control.Lens  (makeLenses)
import Data.Aeson.TH

import Fission.Internal.JSON

data UserConfig = UserConfig { _fissionApiUrl :: Text }
  deriving (Show, Eq)

makeLenses ''UserConfig
$(deriveJSON lens_SCREAMING_SNAKE_CASE ''UserConfig)
