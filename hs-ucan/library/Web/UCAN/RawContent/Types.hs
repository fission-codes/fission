module Web.UCAN.RawContent.Types (RawContent (..)) where

import           Data.Aeson
import           RIO

-- | Newtype wrapper for raw content (i.e. the part that gets signed & verified)
newtype RawContent = RawContent { unRawContent :: Text }
  deriving (Eq, Show)

instance Display RawContent where
  textDisplay = unRawContent

instance ToJSON RawContent where
  toJSON (RawContent txt) = String txt

instance FromJSON RawContent where
  parseJSON = withText "JWT.RawContent" \txt ->
    return $ RawContent txt
