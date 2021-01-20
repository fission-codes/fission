{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Crypto.PubKey.RSA.Error () where

import           Crypto.PubKey.RSA.Types
import qualified RIO.Text                as Text

import           Fission.Prelude

instance Display Error where
  textDisplay = Text.pack . show

instance Exception Error
