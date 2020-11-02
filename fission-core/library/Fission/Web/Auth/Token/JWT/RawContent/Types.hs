module Fission.Web.Auth.Token.JWT.RawContent.Types (RawContent (..)) where

import           Fission.Prelude

-- | Newtype wrapper for raw content (i.e. the part that gets signed & verified)
newtype RawContent = RawContent { unRawContent :: Text }
  deriving (Eq, Show)

instance Display RawContent where
  textDisplay = unRawContent

instance FromJSON RawContent where
  parseJSON = withText "JWT.RawContent" \txt ->
    return $ RawContent txt
