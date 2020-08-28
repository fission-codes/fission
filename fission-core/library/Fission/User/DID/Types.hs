module Fission.User.DID.Types
  ( DID (..)
  -- * Reexport
  , module Fission.User.DID.Method.Types
  ) where

import qualified Data.Aeson.Types as JSON

import           Data.Binary hiding (encode)
import           Data.Base58String.Bitcoin as BS58.BTC
import qualified Data.ByteString.Base64    as BS64

import qualified RIO.ByteString as BS
import qualified RIO.Text       as Text

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Key as Key
import           Fission.User.DID.Method.Types

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

NOTE: The multihash is in a middle-endian format -- SB base-128 with continuation flags
More here: https://github.com/multiformats/unsigned-varint

==== __Examples__

Ed25519

>>> eitherDecode "\"did:key:zStEZpzSMtTt9k2vszgvCwF4fLQQSyA15W5AQ4z3AR6Bx4eFJ5crJFbuGxKmbma4\"" :: Either String DID
Right (DID {method = Key, publicKey = Hv+AVRD2WUjUFOsSNbsmrp9fokuwrUnjBcr92f0kxw4=})

RSA

eitherDecode "\"did:key:z1LBnwEktwYLrpPhuwFowdVwAFX5zpRo9rrVzwiRRBBiaBoCR7hNqgstW7VM3T9auNbqTmQW4vdHb61RhTMVgCp1BTxuaKU3anWoERFXpuZvfE39g8u7HS9BeD1QJN1fX6S8vvskaPhxFkwLtGr4ZffkUE57WZpNWM6U6BqdktZxmKzxC85zN4FC9Ws5LHuGfxaCuBLiUfA7qEYVSz1Qu1rZDpD6NyfUNrHJ1ErZduJun976nbFHrmDnU47RcSMFESbNKDi1467tRfubsMrDzebdCFKQ2m1AYysto2i6Wmcnucjt3tnwTqe7Bo8L1g8h8UGA946DF3WeGbPUGqznsUdLq6XLCmJzJkJmrNyeZkswdyPX2Vu6J9E4J12Sb3h9ds7atByamftitEZsf6hPJkLUXGThYpCnmRAArRRfPY2H6cKDc7AcnFPyNHGkab5ZFo4qvgBZytbK1K5oD3HfQ6E2ybLhyC2b8i5wo6DLtm9ufixSJNNTH7Uikk88CmertKR7s12CK1WLEM3Zu5YBZNphnjj7cp8QTodAehRPV9NG1CLEAMJVN79DvYe6SfiafJobcvfD8npfS6jcejcyouQbEpKDG7QAnKS48P4AvgBqDvfNUe54jMkk6r6CoX4LcYGHukZDEnea9kwkEoXkUYS4j1AfbKh44FzSuXbQqZnjjVpThxCNmmNn1E4qHmsFkvGoF3FN55CPkoGfDCvyQJgqmsFmpeTJSy9wzv4MvbqpuATxr7eyxsGeCWQkcDwub32inGpR3reTfzRJECCFZarnXdcC5Pidakb1Wu8L\""
Right (DID {method = Key, publicKey = MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA+kzeVOVpVWwkWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr/Mrm/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e+lf4s4OxQawWD79J9/5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa+GSYOD2QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9MwIDAQAB})

-}
data DID = DID
  { method    :: !Method
  , publicKey :: !Key.Public
  } deriving (Show, Eq)

instance Arbitrary DID where
  arbitrary = do
    publicKey <- arbitrary
    method    <- arbitrary

    return DID {..}

instance Display DID where -- NOTE `pk` here is base2, not base58
  textDisplay (DID method pk) = header <> UTF8.toBase58Text (BS.pack multicodecW8)
    where
      header :: Text
      header = "did:" <> textDisplay method <> ":" <> "z"

      multicodecW8 :: [Word8]
      multicodecW8 =
        case pk of
          Ed25519PublicKey ed  -> [0xed, 0x01] <> BS.unpack (encodeUtf8 (textDisplay ed))
          RSAPublicKey     rsa -> [0x00, 0xF5, 0x02] <> BS.unpack (BS64.decodeLenient . encodeUtf8 $ textDisplay rsa)
                               {-   ^     ^     ^
                                    |     |     |
                                    |    "expect 373 Bytes", encoded in the mixed-endian format
                                  "raw"
                              -}

instance ToJSON DID where
  toJSON = String . textDisplay

instance FromJSON DID where
  parseJSON = withText "DID" \txt ->
    case Text.stripPrefix "did:key:z" txt of
      Nothing ->
        fail $ show txt <> " does not have a valid did:key header"

      Just fragment -> do
        pk <- case BS.unpack . BS58.BTC.toBytes $ BS58.BTC.fromText fragment of
          (0xed : 0x01 : edKeyW8s) ->
            Ed25519PublicKey <$> parseKeyW8s (BS.pack edKeyW8s)
          
          (0x00 : 0xF5 : 0x02 : rsaKeyW8s) ->
            RSAPublicKey <$> parseKeyW8s (BS64.encode $ BS.pack rsaKeyW8s)
           
          nope ->
            fail . show . BS64.encode $ BS.pack nope <> " is not an acceptable did:key"

        return $ DID Key pk

parseKeyW8s :: FromJSON a => ByteString -> JSON.Parser a
parseKeyW8s = parseJSON . toJSON . decodeUtf8Lenient
