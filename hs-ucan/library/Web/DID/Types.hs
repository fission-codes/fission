module Web.DID.Types
  ( DID (..)
  ) where

import           Data.Aeson                   as JSON
import qualified Data.Aeson.Types             as JSON
import           Data.Swagger

import           Control.Lens                 ((?~))
import           Test.QuickCheck

import           Data.Base58String.Bitcoin    as BS58.BTC
import           Data.Binary                  hiding (encode)
import qualified Data.ByteArray               as BA
import qualified Data.ByteString.Base64       as BS64
import           Data.Hashable                (Hashable (..))

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519        as Ed25519

import           RIO
import qualified RIO.ByteString               as BS
import qualified RIO.Text                     as Text

import qualified Web.UCAN.Internal.UTF8       as UTF8

import           Servant.API

import           Crypto.Key.Asymmetric        as Key (Public (..))
import qualified Crypto.Key.Asymmetric.Public as Public

{- | A DID key, broken into its constituant parts

Format:
did:key:<MULTIBASE(base58-btc, MULTICODEC(public-key-type, raw-public-key-bytes))>

> NOTE: Multibase-prefixes are encoding agnostic. "z" is "z",
> not 0x7a ("z" encoded as ASCII/UTF-8).
> For example, in UTF-32, "z" would be [0x7a, 0x00, 0x00, 0x00].
>
> Expressing a base58btc encoded ed25519 cryptographic identifier would look like this:
> 0x7a 0xed 0x01 ED25519_PUBLIC_KEY_BYTES
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

> eitherDecode (encode "did:key:z6MkgYGF3thn8k1Fv4p4dWXKtsXCnLH7q9yw4QgNPULDmDKB") :: Either String DID
Right (DID.Key Hv+AVRD2WUjUFOsSNbsmrp9fokuwrUnjBcr92f0kxw4=)

RSA

> eitherDecode "\"did:key:z1LBnwEktwYLrpPhuwFowdVwAFX5zpRo9rrVzwiRRBBiaBoCR7hNqgstW7VM3T9auNbqTmQW4vdHb61RhTMVgCp1BTxuaKU3anWoERFXpuZvfE39g8u7HS9BeD1QJN1fX6S8vvskaPhxFkwLtGr4ZffkUE57WZpNWM6U6BqdktZxmKzxC85zN4FC9Ws5LHuGfxaCuBLiUfA7qEYVSz1Qu1rZDpD6NyfUNrHJ1ErZduJun976nbFHrmDnU47RcSMFESbNKDi1467tRfubsMrDzebdCFKQ2m1AYysto2i6Wmcnucjt3tnwTqe7Bo8L1g8h8UGA946DF3WeGbPUGqznsUdLq6XLCmJzJkJmrNyeZkswdyPX2Vu6J9E4J12Sb3h9ds7atByamftitEZsf6hPJkLUXGThYpCnmRAArRRfPY2H6cKDc7AcnFPyNHGkab5ZFo4qvgBZytbK1K5oD3HfQ6E2ybLhyC2b8i5wo6DLtm9ufixSJNNTH7Uikk88CmertKR7s12CK1WLEM3Zu5YBZNphnjj7cp8QTodAehRPV9NG1CLEAMJVN79DvYe6SfiafJobcvfD8npfS6jcejcyouQbEpKDG7QAnKS48P4AvgBqDvfNUe54jMkk6r6CoX4LcYGHukZDEnea9kwkEoXkUYS4j1AfbKh44FzSuXbQqZnjjVpThxCNmmNn1E4qHmsFkvGoF3FN55CPkoGfDCvyQJgqmsFmpeTJSy9wzv4MvbqpuATxr7eyxsGeCWQkcDwub32inGpR3reTfzRJECCFZarnXdcC5Pidakb1Wu8L\""
Right (DID.Key MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA+kzeVOVpVWwkWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr/Mrm/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e+lf4s4OxQawWD79J9/5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa+GSYOD2QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9MwIDAQAB)

-}
data DID
  = Key Key.Public
  -- More varieties here later
  deriving (Show, Eq)

-- For FromJSON
instance Ord DID where
  a `compare` b = textDisplay a `compare` textDisplay b

instance Arbitrary DID where
  arbitrary = Key <$> arbitrary

instance Hashable DID where
  hashWithSalt salt did = hashWithSalt salt $ textDisplay did

instance ToParamSchema DID where
  toParamSchema _ = mempty & type_ ?~ SwaggerString

instance ToHttpApiData DID where
  toUrlPiece = textDisplay

instance FromHttpApiData DID where
  parseUrlPiece txt =
    case eitherDecodeStrict ("\"" <> encodeUtf8 txt <> "\"") of
      Left  err -> Left $ Text.pack err
      Right val -> Right val

instance Display DID where -- NOTE `pk` here is base2, not base58
  textDisplay (Key pk) = "did:key:z" <> UTF8.toBase58Text (BS.pack multicodecW8)
    where
      multicodecW8 :: [Word8]
      multicodecW8 =
        case pk of
          Ed25519PublicKey ed ->
            0xed : 0x01 : BS.unpack (BA.convert ed)

          RSAPublicKey rsa ->
            -- breaking change, but spec-adhering format: 0x85 : 0x24 : BS.unpack (Public.encodeASN1DERRSAPublicKey rsa)
            0x00 : 0xF5 : 0x02 : BS.unpack (BS64.decodeLenient . encodeUtf8 $ textDisplay rsa)

instance ToJSON DID where
  toJSON = String . textDisplay

instance FromJSON DID where
  parseJSON = withText "DID" parseText

instance ToJSONKey DID where
  toJSONKey = JSON.toJSONKeyText textDisplay

instance FromJSONKey DID where
  fromJSONKey = FromJSONKeyTextParser parseText

parseKeyW8s :: FromJSON a => ByteString -> JSON.Parser a
parseKeyW8s = parseJSON . toJSON . decodeUtf8Lenient

parseText :: Text -> JSON.Parser DID
parseText txt =
  case Text.stripPrefix "did:key:z" txt of
    Nothing ->
      fail $ show txt <> " does not have a valid did:key header"

    Just fragment -> do
      pk <- case BS.unpack . BS58.BTC.toBytes $ BS58.BTC.fromText fragment of
        (0xed : 0x01 : edKeyW8s) ->
          if length edKeyW8s > 40
            then
              -- Legacy encoding for backward compatability
              Ed25519PublicKey <$> parseKeyW8s (BS.pack edKeyW8s)

            else
              case Ed25519.publicKey $ BS.pack edKeyW8s of
                CryptoFailed cryptoError -> fail $ "Unable to parse Ed25519 key: " <> show cryptoError
                CryptoPassed edPK -> return $ Ed25519PublicKey edPK

        (0x85 : 0x24 : rsaASNPublicKeyW8s) ->
          case Public.decodeASN1DERRSAPublicKey $ BS.pack rsaASNPublicKeyW8s of
            Right pk -> pure $ RSAPublicKey pk
            Left err -> fail err

        -- Backwards compatibility
        (0x00 : 0xF5 : 0x02 : rsaKeyW8s) ->
          RSAPublicKey <$> parseKeyW8s (BS64.encode $ BS.pack rsaKeyW8s)

        nope ->
          fail . show . BS64.encode $ BS.pack nope <> " is not an acceptable did:key"

      return $ Key pk
