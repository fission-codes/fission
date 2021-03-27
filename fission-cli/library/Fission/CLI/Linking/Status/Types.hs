module Fission.CLI.Linking.Status.Types
  ( Status (..)
  -- * Reexports
  , module Fission.CLI.Linking.Status.Denied.Types
  ) where

import           Fission.Prelude

import           Fission.CLI.Linking.Status.Denied.Types

newtype Status a = Status { unStatus :: a }
  deriving stock (Eq, Show)
  deriving anyclass Exception

instance Display a => Display (Status a) where
  display (Status status) = "Status." <> display status

instance ToJSON a => ToJSON (Status a) where
  toJSON (Status status) =
    object ["linkStatus" .= status]

instance FromJSON a => FromJSON (Status a) where
  parseJSON =
    withObject "LinkStatus" \obj -> do
      status <- obj .: "linkStatus"
      return $ Status status
