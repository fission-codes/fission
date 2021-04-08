{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fission.CLI.Internal.ServerDID (fallback) where

import           Fission.Prelude

import           Fission.User.DID.Types

fallback :: DID
fallback =
  fromJust $ decode "\"did:key:z361CTeTLoCWnoZyMwwZV3myCxm1GRGjaNPmSZCNCqzXo\""
