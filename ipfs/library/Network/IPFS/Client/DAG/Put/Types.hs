module Network.IPFS.Client.DAG.Put.Types (API, Response (..)) where

import           Servant.API
import           Servant.Multipart

import           Network.IPFS.Prelude

import           Network.IPFS.CID.Types
import qualified Network.IPFS.File.Form.Types as File

type API
  =  QueryParam' '[Required, Strict] "pin" Bool
  :> MultipartForm Tmp File.Form
  :> Post '[JSON] Response

newtype Response = Response CID

instance FromJSON Response where
  parseJSON = withObject "IPFS.DAG.Response" \obj -> do
    cidField <- obj .: "Cid"
    cid      <- cidField .: "/"
    return $ Response cid
