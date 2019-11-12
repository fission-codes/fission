{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fission.Internal.Orphanage.UUID () where

import           Data.UUID   as UUID
import qualified RIO.Partial as Partial

import Fission.Prelude

instance Enum UUID

instance Bounded UUID where
  minBound = "00000000-0000-0000-0000-000000000000" |> UUID.fromString |> Partial.fromJust
  maxBound = "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF" |> UUID.fromString |> Partial.fromJust
