{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.String () where

import           RIO
import qualified RIO.Text as Text

instance Display String where
  textDisplay = Text.pack
