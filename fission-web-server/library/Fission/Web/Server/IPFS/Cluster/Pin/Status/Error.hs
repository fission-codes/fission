module Fission.Web.Server.IPFS.Cluster.Pin.Status.Error (Error (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

data Error
  = FailedWith   Text -- ^ An actual error as reported by IPFS Cluster
  | Inconsistent Text -- ^ Not a pinning-related sttatus (because of how IPFS Cluster is set up)
  deriving (Show, Eq)

instance Display Error where
  textDisplay (FailedWith   txt) = "Cluster pin failed with: " <> txt
  textDisplay (Inconsistent txt) = "Cluster pin returned inconsistent state: " <> txt

instance Display [Error] where
  textDisplay errs = Text.intercalate ", " (textDisplay <$> errs)

instance FromJSON Error where
  parseJSON = withText "Cluster.Pin.Error" \txt ->
    case txt of
      "unpinned"     -> return $ Inconsistent "unpinned"
      "unpin_queued" -> return $ Inconsistent "unpin_queued"
      "remote"       -> return $ Inconsistent "remote"
      _              -> return $ FailedWith txt
