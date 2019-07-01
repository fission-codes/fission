module Fission.Web.IPFS.Upload.Error
  ( throwErr
  , throwAdd
  , throwLinear
  ) where

import RIO

import Servant

import Fission.IPFS.Error as IPFSError
import Fission.Internal.Constraint

throwErr :: MonadThrow     m
         => MonadRIO   cfg m
         => HasLogFunc cfg
         => IPFSError.Error -> m a
throwErr = \case
  AddErr           addErr    -> throwAdd addErr
  LinearizationErr linearErr -> throwLinear linearErr

throwAdd :: MonadThrow     m
         => MonadRIO   cfg m
         => HasLogFunc cfg
         => IPFSError.Add
         -> m a
throwAdd = \case
  InvalidFile  -> throwM $ err422 { errBody = "File not processable by IPFS" }
  UnknownError -> throwM $ err500 { errBody = "Unknown IPFS error" }
  UnexpectedOutput txt -> do
    logError $ "Unexpected result from IPFS: " <> display txt
    throwM $ err500 { errBody = "Unexpected IPFS result" }

throwLinear :: MonadThrow     m
            => MonadRIO   cfg m
            => HasLogFunc cfg
            => IPFSError.Linearization
            -> m a
throwLinear (NonLinear sparseTree) = do
    logError $ "Cannot linearize SparseTree: " <> displayShow sparseTree
    throwM $ err500 { errBody = "Unable to linearize IPFS result" }
