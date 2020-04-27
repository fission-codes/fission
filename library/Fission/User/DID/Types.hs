module Fission.User.DID.Types
  ( DID (..)
  -- * Reexport
  , module Fission.User.DID.Method.Types
  ) where

import           Data.Binary hiding (encode)
import           Data.Base58String.Bitcoin as BS58.BTC
import qualified Data.ByteString.Base64 as BS64

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

>>> decode' "\"did:key:zBR4m3DNZHT1G8Nb2RHzgKK7TrWxEmJjZskgvFeJwYJ6kpzy1PVDvn3jR2vaAWExNdtKT7KzBoAdy8GHeGd8jpiAUDgbRRnMy\"" :: Maybe DID
Just (DID {publicKey = Public {publicKey = "AAAAC3NzaC1lZDI1NTE5AAAAIIPnL+R9+OrIm26I1MSOnu4ofAtJ5PjmfiO9ukShjoST"}, algorithm = Ed25519, method = Key})

>>> decode' "\"did:key:z1MdJPaWBebKxtE33AszRWYTF67wCLeFdcsqc3R87hyLKzBKiz49Nyah7i9hzSqMKbQ42UgbfdVFBTRckZECVjkaHTv3QPDKWn1sGRm5GEyzarr4EAT1gUUfXVrwe7satzr3WxZrcpvLzZrviEtV1GhYCr49nyJTn2uamYvozqALP4KKqnR1mgkpo3c8QyZ9DF9HufhXkucFpv8oD5KQWHP8iGhbmqUAWLvTh9CKVx2c2dZWC7cN8VYGWrJYnREUb9t1VptPH15bgVJVVvp1Ho2pervHe37nxoTEM2Ti9cZRKJyUVHdgCjXrpJD4ytSCCSDvTVHXKQitrQTixJoQzBC6dFVKozNUV7eULx5MJq372LQUkz6XJuHK8GgDw8EVNrcmZRDmLVdJGLZDXz3QVJFFQBxDwH7xpd19zciGSoMNnetcAsASMYTx6xCg8u16KE9X8dey38tcSLwREWjaYP8PmmPvVqzBkSsuKw1tSCb7md9axmTP3sKgfyADAcBgk\"" :: Maybe DID
Just (DID {publicKey = Public {publicKey = "AAAAB3NzaC1yc2EAAAADAQABAAABAQDkrRwcO9XZOWdwcK9CUQbzD3NMGlmkoRWu/BS5b/C9lm7PIyjBIhshnd6Y29upBKra7dJ7b1qOJDRQS5uvu93OZi/6pGXcqlYHS9WWJtpEQM+VXeJ2PcnKl5ok2mWgeOEqjHRorT+2dVlISjvOk4dRTJR2sB3el8ynQ1W7LuiEio22352O0DYV89DMhMPVVoSvXVBbsvuJv4VJ4e2XYlilsYyF/6zba4rvEP37MJBExNUqlWUbmIAzFbSoJSdickzHJtLCaBu8Eapu/bu90ecNiFIEaXDSvjD+wVqNwqaarWDor248BULN0u3mVTxHh185k8kBAK6ITBnDMJzjsk11"}, algorithm = RSA2048, method = Key})

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

instance Display DID where
  textDisplay = Text.pack . show

instance ToJSON DID where
  toJSON (DID (Key.Public pk) algo method) = -- NOTE `pk` here is base2, not base58
    String (header <> UTF8.toBase58Text multicodecW8)
    where
      header :: Text
      header = "did:" <> textDisplay method <> ":" <> "z"

      multicodecW8 :: ByteString
      multicodecW8 = BS.pack magicBytes <> pk

      magicBytes :: [Word8]
      magicBytes =
        case algo of
          Ed25519 -> [0xed, 0x01]
          RSA2048 -> [0x00, 0xF5, 0x02]
                  {-   ^     ^     ^
                       |     |     |
                       |    "expect 373 Bytes", encoded in the mixed-endian format
                     "raw"
                  -}

instance FromJSON DID where
  parseJSON = withText "DID" \txt ->
    case Text.stripPrefix "did:key:z" txt of
      Nothing ->
        fail $ show txt <> " does not have a valid did:key header"

      Just fragment ->
        case BS.unpack . BS58.BTC.toBytes $ BS58.BTC.fromText fragment of
          (0xed : 0x01 : edKeyW8s) ->
            return DID
              { publicKey = Key.Public $ BS.pack edKeyW8s
              , algorithm = Ed25519
              , method    = Key
              }

          (0x00 : 0xF5 : 0x02 : rsaKeyW8s) ->
            return DID
              { publicKey = Key.Public . BS64.encode $ BS.pack rsaKeyW8s
              , algorithm = RSA2048
              , method    = Key
              }

          nope ->
            fail $ show nope <> " is not an acceptable did:key"
