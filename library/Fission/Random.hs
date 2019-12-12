-- | Random values
module Fission.Random
  ( alphaNumString
  , byteString
  -- , text
  ) where

import qualified Data.ByteString.Random as BS
import qualified RIO.ByteString         as BS
import qualified Fission.Internal.URL   as URL
import qualified Data.Text            as T
import           Test.RandomStrings
import Fission.Prelude

asciiAlphaNum :: IO Char
asciiAlphaNum = onlyAlphaNum randomASCII

alphaNumString :: Int -> IO Text
alphaNumString len = T.pack <$> randomString asciiAlphaNum len

-- | Generate random 'Text'
text :: Natural -> IO Text
text amount = decodeUtf8Lenient <$> byteString amount

-- | Generate a random 'ByteString'
byteString :: Natural -> IO ByteString
byteString amount = BS.filter URL.isURL <$> BS.random amount