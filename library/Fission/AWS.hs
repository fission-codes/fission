module Fission.AWS
  ( createEnv
  , validate
  , withAWS
  ) where

import Flow
import RIO

import Data.Has
import Servant

import Network.AWS
import Network.AWS.Auth as AWS
import Network.AWS.Route53

import qualified Fission.Config as Config
import           Fission.Internal.Constraint
import           Fission.Web.Error
import           Fission.Internal.Orphanage.ToServerError ()

withAWS :: (MonadUnliftIO m, HasEnv r) => r -> Region -> AWS a -> m a
withAWS env region = runResourceT . runAWS env . within region

createEnv
  :: ( MonadRIO          cfg m
     , Has AWS.AccessKey cfg
     , Has AWS.SecretKey cfg
     )
  => m Env
createEnv = do
  accessKey :: AccessKey <- Config.get
  secretKey :: SecretKey <- Config.get
  liftIO <| newEnv <| FromKeys accessKey secretKey

validate :: ChangeResourceRecordSetsResponse -> Either ServerError ChangeResourceRecordSetsResponse
validate changeSet =
  if status >= 300
    then Left <| toServerError status
    else Right changeSet

  where
    status = changeSet ^. crrsrsResponseStatus
