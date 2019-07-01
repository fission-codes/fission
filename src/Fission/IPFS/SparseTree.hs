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
  Content (CID cid)   -> Right $ textDisplay cid
  Directory [(tag, value)] ->
    case linearize value of
      Left  err         -> Left err
      Right (Path text) -> Right (fromKey tag <> "/" <> text)
  dir -> Left $ Error.NonLinear dir
  where
    fromKey (Hash (CID cid))  = cid
    fromKey (Key (Name name)) = UTF8.textShow name
