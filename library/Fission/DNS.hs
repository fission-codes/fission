module Fission.DNS (splitRecord) where

import           Fission.Prelude

import           RIO.NonEmpty
import qualified RIO.Text as Text
import qualified RIO.List as List


-- if less than 256 bytes, return normal. 
-- if larger, split into chunks & prefix with a 3-char decimal prefix  & semicolon delimiter ("001;")
splitRecord :: Text -> NonEmpty Text
splitRecord txt = 
  if Text.length txt <= txtRecordLimit
    then 
      pure txt

    else 
      fromMaybe (pure "") $ nonEmpty records
      where
        indexed = List.zip [0..] $ Text.chunksOf (txtRecordLimit - 4) txt
        records = joinIndexed <$> indexed


joinIndexed :: (Integer, Text) -> Text
joinIndexed (index, txt) =  
  paddingIndex <> ";" <> txt
  where
    paddingIndex = Text.takeEnd 3 (Text.replicate 2 "0" <> textDisplay index)

txtRecordLimit :: Int
txtRecordLimit = 255
