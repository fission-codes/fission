module Fission.IPFS.SparseTree
  ( SparseTree (..)
  , Error.Linearization (..)
  , linearize
  ) where

import RIO

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Error    as Error
import           Fission.IPFS.Types

linearize :: SparseTree -> Either Error.Linearization Path
linearize = fmap Path . \case
  Stub    (Name name) -> Right $ UTF8.textShow name
  Content (CID _)     -> Right ""
  Directory [(tag, value)] ->
    case linearize value of
      Right (Path "")   -> Right $ fromKey tag
      Right (Path text) -> Right $ fromKey tag <> "/" <> text
      Left  err         -> Left err
  dir -> Left $ Error.NonLinear dir
  where
    fromKey :: Tag -> Text
    fromKey = UTF8.stripN 1 . \case
      Hash (CID cid)   -> cid
      Key  (Name name) -> UTF8.textShow name
