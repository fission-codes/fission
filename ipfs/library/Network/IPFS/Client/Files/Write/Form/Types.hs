module Network.IPFS.Client.Files.Write.Form.Types (Form (..)) where

import           RIO

import           Servant.Multipart
import           Servant.Multipart.API

import           Network.Mime
import qualified Network.IPFS.File.Types as File

data Form = Form
  { content :: File.Serialized
  , fileName :: FileName
  }

instance ToMultipart Mem Form where
  toMultipart Form { content = File.Serialized fileLBS, fileName } =
    MultipartData
      []
      [ FileData
          "data"
          fileName
          (decodeUtf8Lenient $ defaultMimeLookup fileName)
          fileLBS
      ]
