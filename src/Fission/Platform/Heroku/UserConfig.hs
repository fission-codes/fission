{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.Platform.Heroku.UserConfig
  ( UserConfig (..)
  , fissionApiUrl
  , fissionSecret
  , fissionUserName
  ) where

import RIO

import Control.Lens  (makeLenses)
import Data.Aeson.TH

import Database.Selda

import Fission.Internal.JSON
import Fission.Security
import Fission.User

data UserConfig = UserConfig
  { _fissionApiUrl   :: Text
  , _fissionUserName :: ID User
  , _fissionSecret   :: Secret
  }
  deriving (Show, Eq)

makeLenses ''UserConfig
$(deriveJSON lens_SCREAMING_SNAKE_CASE ''UserConfig)
