module Fission.Web.Server.IPFS.Client.DAG
  ( DAGPut
  , Response (..)
  , dagPut
  ) where

import           Network.IPFS
import           Network.IPFS.CID.Types
import qualified Network.IPFS.File.Types            as File

import           Servant.API
import           Servant.Client                     (ClientError, client)
import           Servant.Multipart

import           Fission.Prelude

import qualified Fission.Web.Server.File.Form.Types as File

type DAGPut
  = "api"
  :> "v0"
  :> "dag"
  :> "put"
  :> QueryParam' '[Required, Strict] "pin" Bool
  :> MultipartForm Tmp File.Form
  :> Post '[JSON] Response

newtype Response = Response CID

instance FromJSON Response where
  parseJSON = withObject "IPFS.DAG.Response" \obj -> do
    cidField <- obj .: "Cid"
    cid      <- cidField .: "/"
    return $ Response cid

dagPut :: MonadRemoteIPFS m => File.Serialized -> m (Either ClientError Response)
dagPut file = do
  boundary  <- liftIO genBoundary
  runRemote (dagClient True (boundary, File.Form "file" file)) >>= \case
    Left  err -> return $ Left err
    Right val -> return $ Right val
  where
    dagClient = client (Proxy @DAGPut)
