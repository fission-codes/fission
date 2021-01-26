module Fission.Emoji.Class (ToEmoji (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

class ToEmoji a where
  toEmoji :: a -> Text

instance ToEmoji a => ToEmoji [a] where
  toEmoji xs = Text.intercalate " " (toEmoji <$> xs)
