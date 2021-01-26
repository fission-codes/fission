module Fission.CLI.Digit
  ( toEmoji
  , toEmojiSeq
  , module Fission.CLI.Digit.Types
  ) where

import qualified RIO.Text                as Text

import           Fission.Prelude

import           Fission.CLI.Digit.Types

 -- FIXME shoudl to emoji  be a type class?
toEmoji :: Digit -> Text
toEmoji = \case
  Zero  -> "0️⃣"
  One   -> "1️⃣"
  Two   -> "2️⃣"
  Three -> "3️⃣"
  Four  -> "4️⃣"
  Five  -> "5️⃣"
  Six   -> "6️⃣"
  Seven -> "7️⃣"
  Eight -> "8️⃣"
  Nine  -> "9️⃣"

toEmojiSeq :: [Digit] -> Text
toEmojiSeq ds = Text.intercalate " " (toEmoji <$> ds)
