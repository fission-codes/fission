-- | Raw validation
module Fission.URL.Validation
  ( isValid
  , isURLChar
  ) where

import qualified Data.Char        as Char
import qualified RIO.Text         as Text

import           Fission.Prelude

import qualified Fission.Security as Security

-- | Confirm that a raw is valid
isValid :: Text -> Bool
isValid txt =
  all (== True) preds
  where
    preds :: [Bool]
    preds = [ okChars
            , not blank
            , not startsWithHyphen
            , not endsWithHyphen
            , not startsWithUnderscore
            , not inBlocklist
            ]

    blank = Text.null txt

    inBlocklist = elem txt Security.blocklist
    okChars     = Text.all isURLChar txt

    startsWithHyphen = Text.isPrefixOf "-" txt
    endsWithHyphen   = Text.isSuffixOf "-" txt

    startsWithUnderscore = Text.isPrefixOf "_" txt

isURLChar :: Char -> Bool
isURLChar c =
     Char.isAsciiLower c
  || Char.isDigit      c
  || c == '-'
  || c == '_'
