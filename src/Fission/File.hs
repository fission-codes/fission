module Fission.File (Serialized (..)) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Control.Lens
import qualified Data.ByteString.Builder as Builder
import           Data.Swagger
import           Servant

newtype Serialized = Serialized { unserialize :: Lazy.ByteString }
  deriving        ( Eq
                  , Show
                  , Generic
                  )
  -- deriving anyclass ToSchema
  deriving newtype  IsString

instance ToSchema Serialized where
  declareNamedSchema _ =
     return $ NamedSchema (Just "Serialized File")
            $ mempty & type_ .~ SwaggerString

instance Display Serialized where
  display = Utf8Builder . Builder.lazyByteString . unserialize

instance MimeUnrender PlainText Serialized where
  mimeUnrender _proxy = Right . Serialized

instance MimeUnrender OctetStream Serialized where
  mimeUnrender _proxy = Right . Serialized
