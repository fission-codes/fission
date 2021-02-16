module Fission.Web.Server.IPFS.Cluster.Error.Types
  ( ErrorBody (..)
  , Error (..)
  ) where

import           Fission.Prelude

newtype ErrorBody = ErrorBody {message :: String}
  deriving Show

instance Display ErrorBody where
  display = displayShow

instance FromJSON ErrorBody where
  parseJSON = withObject "ErrorBody" \obj -> do
    message    <- obj .: "message"
    return ErrorBody {..}

data Error
  = ClusterDaemonErr Text
  | UnknownPinErr    Text
  | UnexpectedOutput Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Error where
  display = \case
    ClusterDaemonErr txt -> "Cluster Daemon error: " <> display txt
    UnknownPinErr    txt -> "Unknown IPFS Cluster pin error: " <> display txt
    UnexpectedOutput txt -> "Unexpected IPFS Cluster output: " <> display txt
