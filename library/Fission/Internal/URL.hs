module Fission.Internal.URL
  ( isValidURLCharacter
  , specials
  ) where

import Data.Word8

import Fission.Internal.Bool (anyX)
import Fission.Prelude

-- | Check that a byte represents a valid URL character
isValidURLCharacter :: Word8 -> Bool
isValidURLCharacter = anyX (isAlpha : isDigit : isSpecial)
  where
    isSpecial = (==) <$> specials

-- | List of URL special character checks
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
