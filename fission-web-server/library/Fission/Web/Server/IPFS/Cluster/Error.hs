module Fission.Web.Server.IPFS.Cluster.Error
  ( module Fission.Web.Server.IPFS.Cluster.Error.Types
  , parseClientError
  ) where

import           Fission.Prelude
import           Servant.Client

import           Fission.Internal.Orphanage.ClientError      ()
import           Fission.Web.Server.IPFS.Cluster.Error.Types

-- | Parse and Log the Servant Client Error returned from the IPFS Daemon
parseClientError :: MonadLogger m => ClientError -> m Error
parseClientError err = do
  logError $ displayShow err
  return $ case err of
    FailureResponse _ response ->
      case decode $ responseBody response of
          Just ErrorBody {message} ->
            ClusterDaemonErr $ textShow message

          _ ->
            UnexpectedOutput $ textDisplay err

    unknownClientError ->
      UnknownPinErr $ textDisplay unknownClientError
