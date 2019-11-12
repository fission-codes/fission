-- | File types
module Fission.File.Types (Serialized (..)) where

import qualified Data.ByteString.Builder as Builder
import           Data.Swagger
import           Servant
import qualified RIO.ByteString.Lazy as Lazy

import Fission.Prelude

-- | A file serialized as a lazy bytestring
newtype Serialized = Serialized { unserialize :: Lazy.ByteString }
  deriving         ( Eq
                   , Show
                   )
  deriving newtype ( IsString )

instance ToSchema Serialized where
  declareNamedSchema _ =
    mempty
      |> example     ?~ "hello world"
      |> description ?~ "A typical file's contents"
      |> type_       ?~ SwaggerString
      |> NamedSchema (Just "Serialized File")
      |> pure

instance Display Serialized where
  display = Utf8Builder . Builder.lazyByteString . unserialize

instance MimeRender PlainText Serialized where
  mimeRender _proxy = unserialize

instance MimeUnrender PlainText Serialized where
  mimeUnrender _proxy = Right . Serialized

instance MimeRender OctetStream Serialized where
  mimeRender _proxy = unserialize

instance MimeUnrender OctetStream Serialized where
  mimeUnrender _proxy = Right . Serialized
