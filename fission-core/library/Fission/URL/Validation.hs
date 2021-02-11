-- | Username validation
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

-- | Confirm that a username is valid
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
isValid username =
  all (== True) preds
  where
    preds :: [Bool]
    preds = [ okChars
            , not blank
            , not startsWithHyphen
            , not endsWithHyphen
            , not inBlocklist
            ]

    blank = Text.null username

    inBlocklist = elem username Security.blocklist
    okChars     = Text.all isURLChar username

    startsWithHyphen = Text.isPrefixOf "-" username
    endsWithHyphen   = Text.isSuffixOf "-" username

isURLChar :: Char -> Bool
isURLChar c =
     Char.isAsciiLower c
  || Char.isDigit      c
  || c == '-'

