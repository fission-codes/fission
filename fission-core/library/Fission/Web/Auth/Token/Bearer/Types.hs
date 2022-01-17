-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Token.Bearer.Types
  ( Token     (..)
  , BareToken (..)
  ) where

import qualified RIO.ByteString.Lazy         as Lazy
import qualified RIO.Text                    as Text

import           Data.Aeson.Types
import           Data.Swagger
import           Servant.API

import           Fission.Prelude

import qualified Ucan.Internal.Base64.URL as B64.URL

import qualified Web.JWT.RawContent          as JWT
import           Web.JWT.Types

data Token = Token
  { jwt        :: JWT            -- ^ The actual token
  , rawContent :: JWT.RawContent -- ^ Primarily to pass in to the verifier
  }
  deriving (Show, Eq)

instance Ord Token where
  a `compare` b = textDisplay a `compare` textDisplay b

instance Arbitrary Token where
  arbitrary = do
    jwt@JWT {..} <- arbitrary
    return Token
      { jwt
      , rawContent = RawContent $ B64.URL.encodeJWT header claims
      }

instance Display Token where
  textDisplay = toUrlPiece

instance ToJSON Token where
  toJSON Token {jwt = JWT {sig}, rawContent} =
    String $ "Bearer " <> textDisplay rawContent <> "." <> textDisplay sig

instance FromJSON Token where
  parseJSON = withText "Bearer Token" \txt ->
    case Text.stripPrefix "Bearer " txt <|> Text.stripPrefix "bearer " txt of
      Just rawToken -> do
        jwt <- parseJSON $ toJSON rawToken
        return Token { jwt, rawContent = JWT.contentOf rawToken }

      Nothing ->
        fail $ Text.unpack txt <> " is missing the `Bearer ` prefix"

instance ToHttpApiData Token where
  toUrlPiece token =
    Text.dropEnd 1 . Text.drop 1 . decodeUtf8Lenient . Lazy.toStrict $ encode token

instance FromHttpApiData Token where
  parseUrlPiece txt =
    case Text.stripPrefix "Bearer " txt <|> Text.stripPrefix "bearer " txt of
      Just rawToken ->
        case eitherDecodeStrict . encodeUtf8 $ "\"" <> rawToken <> "\"" of
          Left  str -> Left $ Text.pack str
          Right jwt -> Right Token { jwt, rawContent = JWT.contentOf rawToken }

      Nothing ->
        Left $ txt <> " is missing the `Bearer ` prefix"

--------------
-- newtypes --
--------------

-- | Same as 'Token', but serialized without the 'bearer'. Internal use for UCANs.
newtype BareToken = BareToken Token
  deriving (Eq, Show)

instance Display BareToken where
  textDisplay (BareToken Token {jwt = JWT {sig}, rawContent}) =
    utf8BuilderToText $ display rawContent <> "." <> display sig

instance ToJSON BareToken where
  toJSON (BareToken Token {jwt = JWT {sig}, rawContent}) =
    String $ textDisplay rawContent <> "." <> textDisplay sig

instance FromJSON BareToken where
  parseJSON = withText "Bearer Token" \txt -> do
    jwt <- parseJSON $ toJSON txt
    return $ BareToken Token { jwt, rawContent = JWT.contentOf txt }

instance MimeRender PlainText BareToken where
  mimeRender _ (BareToken Token {jwt = JWT {sig}, rawContent}) =
    buildLazyByteString $ display rawContent <> "." <> display sig

instance MimeRender OctetStream BareToken where
  mimeRender _ (BareToken Token {jwt = JWT {sig}, rawContent}) =
    buildLazyByteString $ display rawContent <> "." <> display sig

instance MimeUnrender PlainText BareToken  where
  mimeUnrender _ lbs = eitherDecode ("\"" <> lbs <> "\"")

instance MimeUnrender OctetStream BareToken  where
  mimeUnrender _ lbs = eitherDecode ("\"" <> lbs <> "\"")

instance ToSchema BareToken where
  declareNamedSchema _ =
    mempty
      |> type_       ?~ SwaggerString
      |> example     ?~ "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsInVhdiI6IjAuMS4wIn0.eyJhdWQiOiJkaWQ6a2V5OnpTdEVacHpTTXRUdDlrMnZzemd2Q3dGNGZMUVFTeUExNVc1QVE0ejNBUjZCeDRlRko1Y3JKRmJ1R3hLbWJtYTQiLCJleHAiOjE1OTE2Mjc4NDEsImlzcyI6ImRpZDprZXk6ejEzVjNTb2cyWWFVS2hkR0NtZ3g5VVp1VzFvMVNoRkpZYzZEdkdZZTdOVHQ2ODlOb0wzazdIdXdCUXhKZFBqdllURHhFd3kxbTh3YjZwOTRjNm1NWTRmTnFBV0RkeVk3NVNWMnFEODVLNU1ZcVpqYUpZOGM0NzFBUEZydkpTZEo1QXo1REVDaENnV0xOdkVYRjJxaFlSMkZWaVprVGhvRVB0Mk5ZcHVxZ0txZVZvS1RpZkRpd3JYeUhka3hCazJNOWNQZE5HaGczRE0xWTZyVGY2RDJIZ2JlUnF2OGtvVW9xeWlLTWpYYUZTZlFZVkY3cGZGWGhOYWdKRGZqcG9xQ3FWWFllaTFQbU43UzM0cFdvcTZwUmRQczY5N2F4VVRFNXZQeXNlU3ROc3o4SGFqdUp2NzlDSENNZ2RvRkJzOXc3UHZNSFVEeGdHUWR0c3hBQXZKRzRxVGJkU0I2bUd2TFp0cGE5czFNemNIcDVEV1NjaUJxNnF1aG1SSHRTVFdHNW9FSnRTdlN4QlhTczJoZFBId0I0QVUiLCJuYmYiOjE1OTE2Mjc3NTEsInByZiI6ImV5SmhiR2NpT2lKU1V6STFOaUlzSW5SNWNDSTZJa3BYVkNJc0luVmhkaUk2SWpBdU1TNHdJbjAuZXlKaGRXUWlPaUprYVdRNmEyVjVPbm94TTFZelUyOW5NbGxoVlV0b1pFZERiV2Q0T1ZWYWRWY3hiekZUYUVaS1dXTTJSSFpIV1dVM1RsUjBOamc1VG05TU0yczNTSFYzUWxGNFNtUlFhblpaVkVSNFJYZDVNVzA0ZDJJMmNEazBZelp0VFZrMFprNXhRVmRFWkhsWk56VlRWakp4UkRnMVN6Vk5XWEZhYW1GS1dUaGpORGN4UVZCR2NuWktVMlJLTlVGNk5VUkZRMmhEWjFkTVRuWkZXRVl5Y1doWlVqSkdWbWxhYTFSb2IwVlFkREpPV1hCMWNXZExjV1ZXYjB0VWFXWkVhWGR5V0hsSVpHdDRRbXN5VFRsalVHUk9SMmhuTTBSTk1WazJjbFJtTmtReVNHZGlaVkp4ZGpocmIxVnZjWGxwUzAxcVdHRkdVMlpSV1ZaR04zQm1SbGhvVG1GblNrUm1hbkJ2Y1VOeFZsaFpaV2t4VUcxT04xTXpOSEJYYjNFMmNGSmtVSE0yT1RkaGVGVlVSVFYyVUhselpWTjBUbk42T0VoaGFuVktkamM1UTBoRFRXZGtiMFpDY3psM04xQjJUVWhWUkhoblIxRmtkSE40UVVGMlNrYzBjVlJpWkZOQ05tMUhka3hhZEhCaE9YTXhUWHBqU0hBMVJGZFRZMmxDY1RaeGRXaHRVa2gwVTFSWFJ6VnZSVXAwVTNaVGVFSllVM015YUdSUVNIZENORUZWSWl3aVpYaHdJam94TlRrME1qRTVOelE0TENKcGMzTWlPaUprYVdRNmEyVjVPbm94TTFZelUyOW5NbGxoVlV0b1pFZERiV2Q0T1ZWYWRWY3hiekZUYUVaS1dXTTJSSFpIV1dVM1RsUjBOamc1VG05TU0wWjFRVlIwVlZCV1p6ZHBXRGQ0YlUxdGNrbzJNVE0wVkcxaE1tSkdWWE5YVVhreVRtUkhTREZYYVRSSWVVNWhOemh3VG0xU1RrRkdVRWhHU21KcVNqRnhZemc0TkZoeE1XbHlhV2wwY1ZKVk0zQmtkRVJSVFhobmIyRjNNVUptVFZkRlRVWnJkVmxWUm5Gd1JFdGlSMHd6WTNKQ2VtNUxkbFpNZDJsRGExQkhiM2h0V0VGWE4waFZOemw2TmxCVVRVZDVVMHRDUlVGMVdERjNSVE0wTlRKQ05rbzVOblphZFc5aVJUTlpZVUV5T0hsTFMwaE1OV2xxWmtKVVJFVTFaRTF6TmpWaFJraGlSbVZUZDNONlJtMWxkVkZxTkdkS1JEVklaMUJOYjI1bGRHZzNUSFZIWlhVMFlWVldWMFZDVkZKdk5sWmlTbFpRTVdKSVNqZHhUbUZRWW1JMFNrUjRTa2hFUkhaeFV6Vm9PRlJCZFZVMmIweFNUa1ZuTTJSdU4xSnBjV295VW1ST1ZYbFRkbkZxVkU0MWQzUk1VMjl3ZGtSNlFVSXllbFl5VUhSYWNETjJNVEZPYjJaRWEyNTJSWFpYYTBONmRYbFdRVnA1VFVGbk9HZGFXa2RESWl3aWJtSm1Jam94TlRreE5qSTNOamc0TENKd2RHTWlPaUpCVUZCRlRrUWlMQ0p6WTNBaU9pSXZJbjAuZWFIcmZuaFJkdy1ob1B3Q2RjVXpTX09abm5VXzFIR3RwU0JHenM1SmtDcGs5RlVQZVYwU2xVbF9kUkI0UzZ0M2xnbi14UHVoaVA3WE5kMGM3Qm9hZFd0SkxMSlY4VV9MZ2pUUUdMa1h4al9yVTZrYXA4bFRibllneDZPZzFiZi10bUhlVVVCNFZOY2RFUGlTMFRUYllMZlFDN05rLTZzbWkwekNYQlVBd1JrLURSQURtdkdhMzloVzFMQlFqWl9oejVhOVRYTTlRb0drS0szQkxDM2JVbXFKU1Y0SjVCTmlxanQ0TktqOUo1RjVYYjdHNTd4MkttWkVMYUw1U2NYV1lvWmpxVlJNWF9MY3k1WE4xaUJvWmFHVHlFbzlVNVBNYzhRSVRoaEdQTXNtc2JTS1cxc1AyUFF6OGNOUHh5b2NXYmZsT0dQZkVGZ21aenk4RGwxTkN3IiwicHRjIjoiQVBQRU5EIiwic2NwIjoiLyJ9"
      |> description ?~ "An encoded UCAN (JWT) *without* a 'bearer' header tag"
      |> NamedSchema (Just "BareUCAN")
      |> pure
