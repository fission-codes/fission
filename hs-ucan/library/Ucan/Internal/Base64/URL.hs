-- | Helpers for working with Base64URL-encoded strings

module Ucan.Internal.Base64.URL
  ( decode
  , encode
  , encodeBS
  , encodeJWT
  , encodeJWTPart
  , addPadding
  ) where

import           Data.Aeson           (ToJSON)
import qualified Data.Aeson           as JSON
import           Data.Word8

import           RIO
import qualified RIO.ByteString       as BS
import qualified RIO.ByteString       as Strict
import qualified RIO.ByteString.Lazy  as Lazy
import qualified RIO.Char             as Char
import qualified RIO.List             as List
import qualified RIO.Text.Partial     as Text.Partial

import qualified Ucan.Internal.Base64 as B64
import qualified Ucan.Internal.UTF8   as UTF8

-- | Go from Base64URL to Base64
decode :: Text -> Text
decode = Text.Partial.replace "-" "+" . Text.Partial.replace "_" "/"

-- | Go from Base64 to Base64URL
encode :: Text -> Text
encode = Text.Partial.replace "+" "-" . Text.Partial.replace "/" "_"

encodeBS :: Strict.ByteString -> Strict.ByteString
encodeBS =
  Strict.map \case
    43  -> 45 -- '+' -> '-'
    47  -> 95 -- '/' -> '_'
    chr -> chr

encodeJWT :: (ToJSON a, ToJSON b) => a -> b -> Text
encodeJWT a b = decodeUtf8Lenient $ encodeJWTPart a <> "." <> encodeJWTPart b

encodeJWTPart :: ToJSON a => a -> ByteString
encodeJWTPart = UTF8.stripPadding . encodeBS . B64.toB64ByteString . Lazy.toStrict . dropWrapper "\"" . JSON.encode
  where
    dropWrapper wrapper bs = dropSuffix wrapper $ dropPrefix wrapper bs
    dropPrefix  prefix  bs = maybe bs id $ Lazy.stripPrefix prefix bs
    dropSuffix  suffix  bs = maybe bs id $ Lazy.stripSuffix suffix bs

addPadding :: ByteString -> ByteString
addPadding bs = BS.pack padded
  where
    n :: Int
    n = rem (BS.length bs) 4

    padded :: [Word8]
    padded = BS.unpack bs <> take n (List.repeat . fromIntegral $ Char.ord '=')
