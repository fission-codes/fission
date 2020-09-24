module Fission.CLI.Environment.OS.Error (Unsupported (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

newtype Unsupported = Unsupported { reportedOS :: String }
  deriving newtype  (Show, Eq)
  deriving anyclass Exception

instance Display Unsupported where
  textDisplay (Unsupported os) = "Unsupported OS: " <> Text.pack os
