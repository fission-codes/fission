module Fission.Web.Server.IPFS.Cluster.Pin.Status.Types (Status (..)) where

import           Fission.Prelude

data Status
  = FailedWith Text
  | Unexpected Text
  | Queued
  | Pinning
  | Pinned
  deriving (Eq, Show)

instance FromJSON Status where
  parseJSON = withObject "Cluster.Pin.Status" \obj -> do
    status   <- obj .: "status"
    errorTxt <- obj .: "error"

    case status :: Text of
      "pinned"       -> return Pinned

      "pinning"      -> return Pinning

      "pin_queued"   -> return Queued
      "queued"       -> return Queued

      "unpinned"     -> return $ Unexpected "unpinned"
      "unpin_queued" -> return $ Unexpected "unpin_queued"
      "remote"       -> return $ Unexpected "remote"

      _              -> return $ FailedWith errorTxt
