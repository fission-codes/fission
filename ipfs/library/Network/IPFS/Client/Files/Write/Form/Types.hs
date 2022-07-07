module Network.IPFS.Client.Files.Write.Form.Types (Form (..)) where

import           RIO
import qualified RIO.ByteString.Lazy     as Lazy

import           Servant.Multipart
import           Servant.Multipart.API

import qualified Network.IPFS.File.Types as File

newtype Form = Form
  { content :: File.Serialized
  }

instance ToMultipart Tmp Form where
  toMultipart Form { content = File.Serialized fileLBS } =
    MultipartData
        [ Input
          { iName  = "data"
          , iValue = decodeUtf8Lenient $ Lazy.toStrict fileLBS
          }
        ]
        []
