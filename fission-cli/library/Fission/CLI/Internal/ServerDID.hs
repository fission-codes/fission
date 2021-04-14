{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fission.CLI.Internal.ServerDID (fallback) where

import           Fission.Prelude

import           Fission.User.DID.Types

fallback :: DID
fallback =
  fromJust $ decode "\"did:key:z6MkgYGF3thn8k1Fv4p4dWXKtsXCnLH7q9yw4QgNPULDmDKB\""
