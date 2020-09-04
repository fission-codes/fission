{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Yaml.ParseException () where

import           Data.Yaml

import           RIO
import qualified RIO.Text  as Text

instance Display ParseException where
  textDisplay = Text.pack . show
