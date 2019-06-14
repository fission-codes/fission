module Fission.Plan (Tier (..)) where

import RIO
import RIO.Text (toLower)

import Control.Lens (makeLenses)
import Data.Aeson

data Tier
  = Test
  | Free
  --  | Paid
  deriving (Show, Eq, Generic)

makeLenses ''Tier

instance ToJSON Tier where
  toJSON = String . toLower . textDisplay . displayShow

instance FromJSON Tier where
  parseJSON (String str) =
    case str of
      "Test" -> pure Test
      "test" -> pure Test

      "Free" -> pure Free
      "free" -> pure Free

      other  -> cantParse other

  parseJSON other = cantParse other

cantParse :: (Monad m, Show a) => a -> m b
cantParse other = fail $ "Unable to parse " <> show other
