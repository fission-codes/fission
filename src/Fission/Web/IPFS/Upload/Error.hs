module Fission.Web.IPFS.Upload.Error (throwErr) where

import RIO

import Servant

import Fission.IPFS.Error as IPFSError
import Fission.Internal.Constraint

throwErr :: MonadThrow     m
         => MonadRIO   cfg m
         => HasLogFunc cfg
         => IPFSError.Add -> m a
throwErr = \case
  InvalidFile  -> throwM $ err422 { errBody = "File not processable by IPFS" }
  UnknownError -> throwM $ err500 { errBody = "Unknown IPFS error" }
  UnexpectedOutput txt -> do
    logError $ "Unexpected result from IPFS: " <> display txt
    throwM $ err500 { errBody = "Unexpected IPFS result" }
