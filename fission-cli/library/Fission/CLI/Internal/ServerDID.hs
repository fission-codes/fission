module Fission.CLI.Internal.ServerDID (fallback) where

import           Fission.Prelude

import           Fission.User.DID.Types

fallback :: DID
fallback =
  fromJust $ decode "\"did:key:zStEZpzSMtTt9k2vszgvCwF4fLQQSyA15W5AQ4z3AR6Bx4eFJ5crJFbuGxKmbma4\""
