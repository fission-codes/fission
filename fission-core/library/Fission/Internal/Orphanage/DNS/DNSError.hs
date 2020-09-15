{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.DNS.DNSError () where

import           Network.DNS
import qualified RIO.Text        as Text

import           Fission.Prelude

instance Display DNSError where
  textDisplay = Text.pack . show
