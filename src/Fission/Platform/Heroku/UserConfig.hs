{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.Platform.Heroku.UserConfig
  ( UserConfig (..)
  , fissionApiUrl
  , fissionSecret
  ) where

import RIO

import Control.Lens  (makeLenses)
import Data.Aeson.TH

import Fission.Internal.JSON
import Fission.Security

data UserConfig = UserConfig
  { _fissionApiUrl :: Text
  -- , _fissionId     :: Word8
  , _fissionSecret :: Secret
  }
  deriving (Show, Eq)

makeLenses ''UserConfig
$(deriveJSON lens_SCREAMING_SNAKE_CASE ''UserConfig)
