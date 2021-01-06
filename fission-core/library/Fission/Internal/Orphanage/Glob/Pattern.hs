{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Glob.Pattern () where

import           Fission.Prelude

import qualified RIO.Text             as Text
import qualified System.FilePath.Glob as Glob

instance ToJSON Glob.Pattern where
  toJSON = String . Text.pack . Glob.decompile

instance FromJSON Glob.Pattern where
  parseJSON = withText "Glob.Pattern" \txt ->
    Glob.compile <$> parseJSON (String txt)
