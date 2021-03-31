module Fission.CLI.Environment.OS.Types (Supported (..)) where

import           Fission.Prelude

data Supported
  = Linux
  | NixOS
  | MacOS
  deriving (Eq, Show)

instance Display Supported where
  display = \case
    Linux -> "Linux"
    NixOS -> "Nix"
    MacOS -> "macOS"
