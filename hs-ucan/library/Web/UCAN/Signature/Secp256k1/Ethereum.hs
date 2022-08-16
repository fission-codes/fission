module Web.UCAN.Signature.Secp256k1.Ethereum (hashWithPrefix, isPublicEthereumSecp256k1Key) where

import           RIO                                  hiding (exp)
import           RIO.ByteString                       as BS
import           RIO.ByteString.Partial               as BS.Partial

import           Crypto.Hash
import qualified Data.ByteArray                       as BA

import qualified Crypto.Secp256k1                     as Secp256k1

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

{-| Check if a given secp256k1 public key is an Ethereum public key.
-}
isPublicEthereumSecp256k1Key :: Secp256k1.PubKeyXY -> Maybe Bool
isPublicEthereumSecp256k1Key key =
  let
    keyBytes =
      Secp256k1.exportPubKeyXY False key
  in
  if BS.length keyBytes == 65 then
    Just (BS.Partial.last keyBytes >= 35)

  else
    -- Can't determine `v` value
    Nothing