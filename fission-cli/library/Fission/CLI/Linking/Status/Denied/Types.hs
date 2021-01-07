module Fission.CLI.Linking.Status.Denied.Types (Denied (..)) where

import           Fission.Prelude

data Denied = Denied
  deriving (Eq, Show)

instance ToJSON Denied where
  toJSON Denied =
    object ["linkStatus" .= ("DENIED" :: Text)]

instance FromJSON Denied where
  parseJSON =
    -- NOTE will likely expand over time, but this is the only one at time of writing
    withObject "LinkStatus" \obj -> do
      status :: Text <- obj .: "linkStatus"
      case status of
        "DENIED" -> return Denied
        _        -> fail "Invalid link status"
