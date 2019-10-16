module Fission.AWS
  ( createEnv
  , ensureContent
  , validate
  , withAWS
  ) where

import RIO

import Data.Has
import Servant

import Network.AWS
import Network.AWS.Auth as AWS
import Network.AWS.Route53

import qualified Fission.Config as Config
import           Fission.Internal.Constraint
import           Fission.Web.Error

withAWS :: (MonadUnliftIO m, HasEnv r) => r -> Region -> AWS a -> m a
withAWS env region = runResourceT . runAWS env . within region

createEnv :: MonadRIO           cfg m
          => Has AWS.AccessKey cfg
          => Has AWS.SecretKey cfg
          => m Env
createEnv = do
  accessKey :: AccessKey <- Config.get
  secretKey :: SecretKey <- Config.get
  liftIO $ newEnv $ FromKeys accessKey secretKey

validate :: ChangeResourceRecordSetsResponse -> Either ServerError ChangeResourceRecordSetsResponse
validate changeSet =
  if status >= 300
    then Left $ toServerError status
    else Right changeSet

  where
    status = changeSet ^. crrsrsResponseStatus

-- | Ensure that a request completed, and that the status code is not in an error range
ensureContent
  :: MonadRIO   cfg m
  => MonadThrow     m
  => Exception err
  => m (Either err ChangeResourceRecordSetsResponse)
  -> m ChangeResourceRecordSetsResponse
ensureContent runRequest = do
  errOrResp <- runRequest
  resp      <- ensureM errOrResp
  ensureM $ validate resp
