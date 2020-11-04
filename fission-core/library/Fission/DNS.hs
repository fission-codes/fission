module Fission.DNS
  ( splitRecord
  , mergeSegments
  ) where

import           Fission.Prelude

import qualified RIO.List        as List
import           RIO.NonEmpty    as NonEmpty
import qualified RIO.Text        as Text

-- If less than 256 bytes, return normal.
-- If larger, split into chunks & prefix with a 3-char decimal prefix  & semicolon delimiter ("001;")
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

-- NOTE a bit information lossy, could be improved
mergeSegments :: NonEmpty Text -> Text
mergeSegments txts =
  txts
    |> fmap toIndexed
    |> NonEmpty.sortBy (\(x, _) (y, _) -> compare x y)
    |> foldr (\(_, txt) acc -> txt <> acc) ""

toIndexed :: Text -> (Natural, Text)
toIndexed txt =
  case (delim, readMaybe (Text.unpack num)) of
    (";", Just idx) -> (idx, body)
    _               -> (0,   txt)

  where
    (rawIndex, body)  = Text.splitAt 4 txt
    (num,      delim) = Text.splitAt 3 rawIndex
