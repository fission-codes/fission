module Fission.CLI.Linking.Status.Denied.Types (Denied (..)) where

import           Fission.Prelude

data Denied = Denied
  deriving (Eq, Show, Exception)

instance Display Denied where
  display _ = "Denied"

-- NOTE will likely expand over time, but this is the only one at time of writing,
-- so just serializing directly here FIXME?
instance ToJSON Denied where
  toJSON Denied =
    object ["linkStatus" .= ("DENIED" :: Text)]

instance FromJSON Denied where
  parseJSON =
    withObject "LinkStatus" \obj -> do
      status :: Text <- obj .: "linkStatus"
      case status of
        "DENIED" -> return Denied
        _        -> fail "Invalid link status"
