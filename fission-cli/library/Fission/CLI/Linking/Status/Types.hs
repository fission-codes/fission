module Fission.CLI.Linking.Status.Types (Status (..)) where

import           Fission.Prelude

data Status
  = Denied
  deriving (Eq, Show)

instance ToJSON Status where
  toJSON Denied =
    object ["linkStatus" .= ("DENIED" :: Text)]

instance FromJSON Status where
  parseJSON =
    withObject "LinkStatus" \obj -> do
      status :: Text <- obj .: "linkStatus"
      case status of
        "DENIED" -> return Denied
        _        -> fail "Invalid link status"
