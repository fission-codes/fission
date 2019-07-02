module Fission.IPFS.SparseTree
  ( SparseTree (..)
  , Error.Linearization (..)
  , linearize
  ) where

import RIO

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Error    as Error

import Fission.IPFS.CID.Types
import Fission.IPFS.Name.Types
import Fission.IPFS.Path.Types
import Fission.IPFS.SparseTree.Types

linearize :: SparseTree -> Either Error.Linearization Path
linearize = fmap (Path . wrap "\"") . go
  where
  wrap :: Text -> Text -> Text
  wrap outside inside = outside <> inside <> outside

  go :: SparseTree -> Either Error.Linearization Text
  go = \case
    Stub      (Name name)    -> Right $ UTF8.textShow name
    Content   (CID _)        -> Right ""
    Directory [(tag, value)] -> fromPath tag <$> go value
    badDir                   -> Left $ Error.NonLinear badDir
    where
      fromPath tag ""   = fromKey tag
      fromPath tag text = fromKey tag <> "/" <> text

      fromKey :: Tag -> Text
      fromKey = UTF8.stripN 1 . \case
        Hash (CID cid)   -> cid
        Key  (Name name) -> UTF8.textShow name
