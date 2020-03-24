module Fission.Internal.URL
  ( isURLCharacter
  ) where

import Data.Word8

import Fission.Internal.Bool (anyX)
import Fission.Prelude

{-| Check that a byte represents a valid URL character.

    $setup
    >>> import Data.Char (ord)
    >>> fromChar = ord .> fromIntegral

    >>> isURLCharacter (fromChar 'a')
    True

    >>> isURLCharacter (fromChar '/')
    False

    >>> isURLCharacter (fromChar '?')
    False

-}
isURLCharacter :: Word8 -> Bool
isURLCharacter w
  =  isAsciiUpper w
  || isAsciiLower w
  || isDigit w
  || anyX urlSpecials w

-- | List of URL special character checks
urlSpecials :: [Word8 -> Bool]
urlSpecials =
  fmap (==)
    [ _asterisk
    , _comma
    , _exclam
    , _hyphen
    , _parenleft
    , _parenright
    , _period
    , _plus
    , _underscore
    ]
