module Fission.CLI.Linking.Status.Denied.Types (Denied (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

data Denied = Denied
  deriving (Eq, Show, Exception)

instance Display Denied where
  display _ = "Denied"

instance ToJSON Denied where
  toJSON Denied = String "DENIED"

instance FromJSON Denied where
  parseJSON =
    withText "Denied" \txt -> do
      case Text.toUpper txt of
        "DENIED" -> return Denied
        _        -> fail "Not the Denied status"
