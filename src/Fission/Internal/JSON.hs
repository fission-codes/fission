{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Internal.JSON
  ( lens_snake_case
  , lens_SCREAMING_SNAKE_CASE
  , snake_case
  , _SCREAMING_SNAKE_CASE
  ) where

import RIO
import RIO.Char (toUpper)

import Data.Aeson
import Data.Aeson.Casing

snake_case :: Options
snake_case = defaultOptions { fieldLabelModifier = snakeCase }

lens_snake_case :: Options
lens_snake_case = defaultOptions { fieldLabelModifier = drop 1 . snakeCase }

_SCREAMING_SNAKE_CASE :: Options
_SCREAMING_SNAKE_CASE = defaultOptions
  { fieldLabelModifier = (fmap toUpper) . snakeCase }

lens_SCREAMING_SNAKE_CASE :: Options
lens_SCREAMING_SNAKE_CASE = defaultOptions
  { fieldLabelModifier = drop 1 . (fmap toUpper) . snakeCase }
