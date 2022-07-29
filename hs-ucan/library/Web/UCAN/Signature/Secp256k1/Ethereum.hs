module Web.UCAN.Signature.Secp256k1.Ethereum (hashWithPrefix) where

import           RIO                                  hiding (exp)
import           RIO.ByteString                       as BS

import           Crypto.Hash
import qualified Data.ByteArray                       as BA

{-| Adds the 'Ethereum signed message' prefix and hashes the content with Keccak 256.
-}
hashWithPrefix :: ByteString -> ByteString
hashWithPrefix content =
  let
    contentLength =
        textDisplay (BS.length content)

    prefix =
      0x19 : (BS.unpack $ encodeUtf8 $ "Ethereum Signed Message:\n" <> contentLength)

    prefixedContent =
      BS.pack prefix <> content
  in
  BS.pack . BA.unpack $ (hash prefixedContent :: Digest Keccak_256)