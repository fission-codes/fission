module Fission.Internal.URL
  ( isURL
  , specials
  ) where

import Data.Word8

import Fission.Internal.Bool (anyX)
import Fission.Prelude

isURL :: Word8 -> Bool
isURL = anyX (isAlpha : isDigit : isSpecial)
  where
    isSpecial = (==) <$> specials

specials :: [Word8]
specials =
    [ _asterisk
    , _comma
    , _dollar
    , _exclam
    , _hyphen
    , _parenleft
    , _parenright
    , _period
    , _plus
    , _underscore
    ]
