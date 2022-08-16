{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fission.Internal.Fixture.Key.Ed25519
  ( pk
  ) where

import           Data.Aeson      as JSON

import           Fission.Prelude
import qualified Fission.Key     as Key

pk :: Key.Public
Success pk = fromJSON . JSON.object $ [( "type", "Ed25519" ), ( "key", "Hv+AVRD2WUjUFOsSNbsmrp9fokuwrUnjBcr92f0kxw4=" )]
