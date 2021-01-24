module Fission.JSON.Error (Error (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude hiding (Error)

newtype Error = Error String
  deriving newtype (Eq, Show)
  deriving anyclass Exception

instance Display Error where
  textDisplay (Error str) = "JSON error: " <> Text.pack str
