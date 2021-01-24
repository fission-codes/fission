{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Crypto.Error () where

import           Crypto.Error
import qualified RIO.Text        as Text

import           Fission.Prelude

instance Display CryptoError where
  textDisplay = Text.pack . show
