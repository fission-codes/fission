module Network.IPFS.SparseTree
  ( SparseTree (..)
  , Error.Linearization (..)
  , linearize
  , cIDs
  ) where

import           Network.IPFS.Prelude
import qualified Network.IPFS.Internal.UTF8 as UTF8
import qualified Network.IPFS.Error    as Error

import           Network.IPFS.CID.Types
import           Network.IPFS.Name.Types
import           Network.IPFS.Path.Types
import           Network.IPFS.SparseTree.Types

linearize :: SparseTree -> Either Error.Linearization Path
linearize = fmap Path . go
  where
  go :: SparseTree -> Either Error.Linearization Text
  go = \case
    Stub      (Name name)    -> Right <| UTF8.textShow name
    Content   (CID _)        -> Right ""
    Directory [(tag, value)] -> fromPath tag <$> go value
    badDir                   -> Left <| Error.NonLinear badDir
    where
      fromPath tag ""   = fromKey tag
      fromPath tag text = fromKey tag <> "/" <> text

      fromKey :: Tag -> Text
      fromKey = UTF8.stripN 1 . \case
        Hash (CID cid)   -> cid
        Key  (Name name) -> UTF8.textShow name

-- | Get all CIDs from a 'SparseTree' (all levels)
cIDs :: (Monoid (f CID), Applicative f) => SparseTree -> f CID
cIDs (Stub _)       = mempty
cIDs (Content cid)  = pure cid
cIDs (Directory kv) = foldMap cIDs kv
