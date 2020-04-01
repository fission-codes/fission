module Fission.Web.Auth.JWT.Types
  ( JWT (..)

  -- * reexports
 
  , module Fission.Web.Auth.JWT.Claims.Types
  , module Fission.Web.Auth.JWT.Header.Types
  ) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified RIO.ByteString.Lazy        as Lazy

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed

import           Fission.Prelude
 
import           Fission.Key.Asymmetric.Algorithm.Types

import           Fission.Web.Auth.JWT.Claims.Types
import           Fission.Web.Auth.JWT.Header.Types
import           Fission.Web.Auth.JWT.Signature.Types

data JWT = JWT
  { header :: !Header
  , claims :: !Claims
  , sig    :: !Signature
  } deriving (Show, Eq)

instance ToJSON JWT where
  toJSON JWT {..} = String encoded
    where
      encoded :: Text
      encoded = decodeUtf8Lenient (toStrictBytes payload) <> signed

      signed :: Text
      signed = utf8BuilderToText $ displayShow sig

      payload :: Lazy.ByteString
      payload = encode header <> "." <> encode claims <> "."

instance FromJSON JWT where
  parseJSON = withText "JWT.Token" \txt ->
    case Char8.split '.' (Lazy.fromStrict $ encodeUtf8 txt) of
      [header', claims', sig'] -> do
        let
          result = do
            header <- eitherDecode header'
            claims <- eitherDecode claims'
            sig    <- errOrSig (alg header) sig'
            return JWT {..}

        case result of
          Left  err   -> fail err
          Right token -> return token

      _ ->
        fail $ show txt <> " is not a valid JWT.Token"
    where
      -- FIXME add RSA case
      errOrSig Ed25519 sig'' =
        case Ed.signature (Lazy.toStrict sig'') of
          CryptoFailed err ->
            Left $ show sig'' <> " is not a valid signature. " <> show err

          CryptoPassed s ->
            Right s
