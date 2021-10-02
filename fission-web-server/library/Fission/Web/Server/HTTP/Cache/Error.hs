module Fission.Web.Server.HTTP.Cache.Error (ResponseError (..), BatchErrors (..)) where

import qualified RIO.Text                               as Text

import           Fission.Prelude
import           Fission.URL

import           Fission.Web.Server.StatusCode.Types

import           Fission.Internal.Orphanage.ClientError ()

data ResponseError = ResponseError
  { url        :: URL
  , statusCode :: StatusCode
  , body       :: Maybe ByteString
  }
  deriving (Show, Eq)

instance Display ResponseError where
  textDisplay ResponseError {..} =
    textDisplay url <> " ("<> textDisplay statusCode <> ")=" <> bodyTxt
    where
      bodyTxt =
        case body of
          Nothing -> "No body"
          Just bs -> decodeUtf8Lenient bs

newtype BatchErrors = BatchErrors { errors :: NonEmpty ResponseError }
  deriving (Show, Eq)

instance Display BatchErrors where
  textDisplay BatchErrors {errors} =
    "HTTP.Cache.BatchErrors=[" <> filling <> "]"
    where
      filling =
        errors
          |> foldr (\x acc -> textDisplay x : acc) []
          |> Text.intercalate ", "
