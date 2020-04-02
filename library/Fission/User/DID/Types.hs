module Fission.User.DID.Types
  ( DID (..)
  -- * Reexport
  , module Fission.User.DID.Method.Types
  ) where

import           Data.Binary hiding (encode)
import           Data.Base58String.Bitcoin as BS58.BTC

import qualified RIO.ByteString      as BS
import qualified RIO.ByteString.Lazy as BS.Lazy
import qualified RIO.Text            as Text

import           Fission.Prelude
import           Fission.Key as Key
 
import           Fission.User.DID.Method.Types
import qualified Fission.Internal.UTF8 as UTF8

{- | A DID key, broken into its constituant parts

Format:
did:key:<MULTIBASE(base58-btc, MULTICODEC(public-key-type, raw-public-key-bytes))>

> NOTE: Multibase-prefixes are encoding agnostic. "z" is "z",
> not 0x7a ("z" encoded as ASCII/UTF-8).
> For example, in UTF-32, "z" would be [0x7a, 0x00, 0x00, 0x00].
>
> Expressing a base58btc encoded ed25519 cryptographic identifier would look like this:
> 0x7a 0xed01 ED25519_PUBLIC_KEY_BYTES
> [...]
> Expressing a cryptographic identifier that is a base58btc encoded RSA SPKI-based
>   public key fingerprint using SHA2-256/256 would look like this:
> 0x7a 0x5d 0x12 0x20 HASH_BYTES
https://github.com/w3c-ccg/lds-ed25519-2018/issues/3

> So the procedure to express other types of keys is:
>
>    1. Find the multicodec representation for your key type.
>         For example, the registration for secp256k1 keys is pending
>         over at multiformats/multicodec#160.
>    2. Encode the raw public key bytes using that multicodec value.
>    3. Pick a character encoding (I suggest base58-btc),
>         and convert it to a character string using Multibase.
>    4. Prepend did:key: to that string, and there you have it!
https://github.com/w3c-ccg/did-method-key/issues/3#issuecomment-594190524

NOTE: The multihash is in little-endian base-7 with continuation flags
More here: https://github.com/multiformats/unsigned-varint
-}
data DID = DID
  { publicKey :: !Key.Public
  , algorithm :: !Key.Algorithm
  , method    :: !Method
  } deriving (Show, Eq)

instance Arbitrary DID where
  arbitrary = do
    publicKey <- arbitrary
    algorithm <- arbitrary
    method    <- arbitrary

    return DID {..}

instance ToJSON DID where
  toJSON (DID (Key.Public pk) algo method) =
    String (header <> UTF8.toBase58Text multicodec64)
    where
      header :: Text
      header = "did:" <> textDisplay method <> ":" <> "z"

      multicodec64 :: ByteString
      multicodec64 = BS.pack magicBytes <> encodeUtf8 pk

      magicBytes :: [Word8]
      magicBytes =
        case algo of
          Ed25519 -> [0xed, 0x01]
          RSA2048 -> [0x00, 0x75, 0x01]

instance FromJSON DID where
  parseJSON = withText "DID" \txt ->
    case Text.stripPrefix "did:key:z" txt of
      Nothing ->
        fail $ show txt <> " does not have a valid did:key header"

      Just fragment ->
        case decode' . BS.Lazy.fromStrict . encodeUtf8 $ fragment of
          Nothing ->
            fail $ show fragment <> " is not a properly formatted multicodec"
           
          Just b58 ->
            case (BS58.BTC.toBinary b58 :: [Word8]) of
              (0xed : 0x01 : edKeyW8s) ->
                return DID
                  { publicKey = Key.Public $ UTF8.toBase58Text edKeyW8s
                  , algorithm = Ed25519
                  , method    = Key
                  }

              (0x00 : 0x75 : 0x01 : rsaKeyW8s) ->
                return DID
                  { publicKey = Key.Public $ UTF8.toBase58Text rsaKeyW8s
                  , algorithm = RSA2048
                  , method    = Key
                  }

              nope ->
                fail $ show nope <> " is not an acceptable did:key"

