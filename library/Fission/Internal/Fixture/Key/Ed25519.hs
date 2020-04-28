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
rawPK = "1498b5467a63dffa2dc9d9e069caf075d16fc33fdd4c3b01bfadae6433767d93"
