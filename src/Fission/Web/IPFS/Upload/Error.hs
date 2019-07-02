module Fission.Web.IPFS.Upload.Error
  ( throwErr
  , throwAdd
  , throwLinear
  ) where

import RIO

import Servant
import Servant.Exception

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

instance ToServantErr IPFSError.Linearization where
  status  _ = 500
  message _ = "Unable to linearize IPFS result"

instance ToServantErr IPFSError.Add where
  status = \case
    InvalidFile        -> 422
    UnknownError       -> 500
    UnexpectedOutput _ -> 500

  message = \case
    InvalidFile        -> "File not processable by IPFS"
    UnknownError       -> "Unknown IPFS error"
    UnexpectedOutput _ -> "Unexpected IPFS result"

throwRIO :: MonadRIO   cfg m
         => HasLogFunc cfg
         => MonadThrow     m
         => Display      err
         => ToServantErr err
         => err
         -> m ()
throwR err = do
  let servErr = toServantErr err
  when (errHTTPCode servErr >= 500) (logError $ display err)
  throwM $ toServantErr err
