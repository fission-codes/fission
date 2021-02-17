module Fission.Web.Server.IPFS.Cluster.Pin.Status.Types
  ( Status (..)
  , module Fission.Web.Server.IPFS.Cluster.Pin.Status.Error
  , module Fission.Web.Server.IPFS.Cluster.Pin.Status.Lifecycle.Types
  ) where

import           Fission.Prelude

import           Fission.Web.Server.IPFS.Cluster.Pin.Status.Error
import           Fission.Web.Server.IPFS.Cluster.Pin.Status.Lifecycle.Types

data Status
  = Normal Lifecycle
  | Failed Error
  deriving (Show, Eq)

instance FromJSON Status where
  parseJSON = withObject "Cluster.Pin.Status" \obj -> do
    let
      getStatus = Right <$> obj .: "status"
      getError  = Left  <$> obj .: "error"

    statusOrError <- getStatus <|> getError

    case statusOrError of
      Right lc -> return $ Normal lc
      Left err -> return $ Failed err
