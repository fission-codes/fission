-- | 

module Fission.User.DID.Types
  ( DID    (..)
  , Method (..)
  , module Fission.PublicKey.Types
  ) where

import           Data.Binary
import           Data.Base58String.Bitcoin as BS58.BTC

import qualified RIO.ByteString as BS
import qualified RIO.Text       as Text

import           Fission.Prelude
import           Fission.PublicKey.Types

-- | A DID key, broken into its constituant parts
--
-- Format:
-- did:key:<MULTIBASE(base58-btc, MULTICODEC(public-key-type, raw-public-key-bytes))>
--
-- > NOTE: Multibase-prefixes are encoding agnostic. "z" is "z",
-- > not 0x7a ("z" encoded as ASCII/UTF-8).
-- > For example, in UTF-32, "z" would be [0x7a, 0x00, 0x00, 0x00].
-- >
-- > Expressing a base58btc encoded ed25519 cryptographic identifier would look like this:
-- > 0x7a 0xed01 ED25519_PUBLIC_KEY_BYTES
-- > [...]
-- > Expressing a cryptographic identifier that is a base58btc encoded RSA SPKI-based
-- >   public key fingerprint using SHA2-256/256 would look like this:
-- > 0x7a 0x5d 0x12 0x20 HASH_BYTES
-- https://github.com/w3c-ccg/lds-ed25519-2018/issues/3
--
-- > So the procedure to express other types of keys is:
-- >
-- >    1. Find the multicodec representation for your key type.
-- >         For example, the registration for secp256k1 keys is pending
-- >         over at multiformats/multicodec#160.
-- >    2. Encode the raw public key bytes using that multicodec value.
-- >    3. Pick a character encoding (I suggest base58-btc),
-- >         and convert it to a character string using Multibase.
-- >    4. Prepend did:key: to that string, and there you have it!
-- https://github.com/w3c-ccg/did-method-key/issues/3#issuecomment-594190524
--
-- NOTE: The multihash is in little-endian base-7 with continuation flags
-- More here:https://github.com/multiformats/unsigned-varint
data DID = DID
  { publicKey :: !PublicKey
  , algorithm :: !Algorithm
  , method    :: !Method
  } deriving (Show, Eq)

instance ToJSON DID where
  toJSON (DID pk algo method) = String (header <> keyType <> pk58)
    where
      header :: Text
      header = "did:" <> textDisplay method <> ":" <> "z"

      keyType :: Text
      keyType = toBase58Text keyTypeRaw

      keyTypeRaw :: [Word8]
      keyTypeRaw =
        case algo of
          Ed25519 -> [0xed, 0x01]
          RSA2048 -> [0x00, 0x75, 0x01]

      pk58 :: Text
      pk58 = toBase58Text pk

      toBase58Text :: Binary a => a -> Text
      toBase58Text = decodeUtf8Lenient . BS58.BTC.toBytes . BS58.BTC.fromBinary

-- b1111010100000001 = F501 -- FIXME PLEASE DOUBLE CHECK THE MATH

-- FIXME: THIS NEED A BUUUUUNCH OF TESTS (confusing encodnig)

instance FromJSON DID where
  parseJSON = withText "DID" \txt ->
    case Text.stripPrefix "did:key:z" txt of
      Nothing ->
        error $ show txt <> " does not have a valid did:key header"

      Just fragment ->
        fragment
          |> BS58.BTC.fromText
          |> BS58.BTC.toBytes
          |> BS.unpack
          |> \case
            (0xed : 0x01 : edKeyW8s) ->
              return DID
                { publicKey = PublicKey $ foo edKeyW8s
                , algorithm = Ed25519
                , method    = Key
                }

            (0x00 : 0x75 : 0x01 : rsaKeyW8s) ->
              return DID
                { publicKey = PublicKey $ foo rsaKeyW8s
                , algorithm = RSA2048
                , method    = Key
                }

            nope ->
              fail $ show nope <> " is not an acceptable did:key"

foo :: [Word8] -> Text
foo = decodeUtf8Lenient . BS.pack

data Method
  = Key
  deriving (Show, Eq)

instance Display Method where
  display Key = "key"

instance ToJSON Method where
  toJSON Key = "key"

instance FromJSON Method where
  parseJSON = withText "DID.Method" \case
    "key" -> return Key
    other -> fail $ show other <> " is not an acceptable DID method"
