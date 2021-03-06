module Fission.Web.Server.File.Form.Types (Form (..)) where

import qualified Network.IPFS.File.Types as File
import qualified RIO.ByteString.Lazy     as Lazy

import           Servant.Multipart

import           Fission.Prelude

data Form = Form
  { name       :: Text
  , serialized :: File.Serialized
  }

instance ToMultipart Tmp Form where
  toMultipart Form { name = name, serialized = File.Serialized fileLBS } =
    MultipartData
        [ Input
          { iName  = name
          , iValue = decodeUtf8Lenient $ Lazy.toStrict fileLBS
          }
        ]
        []
