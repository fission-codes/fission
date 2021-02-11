-- | Raw validation
module Fission.URL.Validation
  ( isValid
  , isURLChar
  ) where

import qualified Data.Char        as Char
import qualified RIO.Text         as Text

import           Fission.Prelude

import qualified Fission.Security as Security

-- $setup
-- >>> :set -XOverloadedStrings

-- | Confirm that a raw is valid
--
-- >>> isValid "simple"
-- True
--
-- >>> isValid "happy-name"
-- True
--
-- Blocklisted words are not allowed
--
-- >>> isValid "recovery"
-- False
--
-- They're not case sensitive
--
-- >>> isValid "reCovErY"
-- False
--
-- They can't contain uppercase characters at all
--
-- >>> isValid "hElLoWoRlD"
-- False
--
-- Nor are various characters
--
-- >>> isValid "under_score"
-- False
--
-- >>> isValid "plus+plus"
-- False
--
-- >>> isValid "-startswith"
-- False
--
-- >>> isValid "endswith-"
-- False
--
-- >>> isValid "with.space"
-- False
--
-- >>> isValid "with.dot"
-- False
--
-- >>> isValid "has.two.dots"
-- False
--
-- >>> isValid "name&with#chars"
-- False
isValid :: Text -> Bool
isValid txt =
  all (== True) preds
  where
    preds :: [Bool]
    preds = [ okChars
            , not blank
            , not startsWithHyphen
            , not endsWithHyphen
            , not inBlocklist
            ]

    blank = Text.null txt

    inBlocklist = elem txt Security.blocklist
    okChars     = Text.all isURLChar txt

    startsWithHyphen = Text.isPrefixOf "-" txt
    endsWithHyphen   = Text.isSuffixOf "-" txt

isURLChar :: Char -> Bool
isURLChar c =
     Char.isAsciiLower c
  || Char.isDigit      c
  || c == '-'
