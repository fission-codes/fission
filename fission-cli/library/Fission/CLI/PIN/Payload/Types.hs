module Fission.CLI.PIN.Payload.Types (Payload (..)) where

import           Fission.Prelude

import           Fission.Emoji.Class
import           Fission.User.DID.Types

import           Fission.CLI.Digit.Types
import           Fission.CLI.PIN.Types

-- | Used in linking flow
data Payload = Payload
  { did :: DID
  , pin :: PIN
  }
  deriving (Eq, Show)

instance ToEmoji Payload where
  toEmoji Payload {pin = PIN {..}} = toEmoji ([a, b, c, d, e, f] :: [Digit])

instance ToJSON Payload where
  toJSON Payload {did, pin} =
    object [ "did" .= did
           , "pin" .= pin
           ]

instance FromJSON Payload where
  parseJSON = withObject "Payload" \obj -> do
    did <- obj .: "did"
    pin <- obj .: "pin"
    return Payload {..}
