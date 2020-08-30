module Fission.CLI.Error.Types
  ( NotRegistered (..)
  , NoKeyFile     (..)
  ) where

import           Fission.Prelude

data NotRegistered = NotRegistered
  deriving (Show, Eq, Exception)

instance Display NotRegistered where
  display NotRegistered = "Not registered"

----------

data NoKeyFile = NoKeyFile
  deriving (Show, Eq, Exception)

instance Display NoKeyFile where
  display NoKeyFile = "No key file"
