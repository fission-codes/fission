{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fission.Internal.Fixture.Key.Ed25519
  ( pk
  , rawPK
  ) where

import           Servant.API

import           Fission.Prelude
import qualified Fission.Key as Key

pk :: Key.Public
Right pk = parseUrlPiece rawPK

rawPK :: Text
rawPK = "Hv+AVRD2WUjUFOsSNbsmrp9fokuwrUnjBcr92f0kxw4="
