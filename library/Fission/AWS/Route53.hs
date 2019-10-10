module Fission.AWS.Route53 
  ( createChangeRequest
  , registerDomain
  ) where

import RIO
import Servant

import Data.Has
import qualified Fission.Config as Config

import Network.AWS.Route53 as Route53
import Network.AWS.Prelude as AWS
import Network.AWS.Auth    as AWS
import Fission.AWS.Types   as AWS
import Control.Monad.Trans.AWS

import Fission.Internal.Constraint


import Control.Lens ((?~))

registerDomain :: MonadRIO       cfg m
            => HasLogFunc cfg
           => Has AWS.AccessKey  cfg
           => Has AWS.SecretKey  cfg
           => Has AWS.ZoneId     cfg
           => RecordType
           -> Text
           -> Text
           -> m(Maybe ServerError)
registerDomain recordType domain content = do 
  logDebug $ "DNS record at: " <> displayShow domain
  env <- createEnv
  req <- createChangeRequest recordType domain content

  liftIO . runResourceT . runAWST env . within NorthVirginia $ do
    res <- send req
    return . parseStatus $ res ^. crrsrsResponseStatus

createEnv :: MonadRIO           cfg m
           => Has AWS.AccessKey cfg
           => Has AWS.SecretKey cfg
           => m(Env)
createEnv = do
  AccessKey accessKey <- Config.get
  SecretKey secretKey <- Config.get
  liftIO $ newEnv $ FromKeys (AccessKey accessKey) (SecretKey secretKey)

createChangeRequest :: MonadRIO       cfg m
                    => Has AWS.ZoneId cfg
                    => RecordType
                    -> Text
                    -> Text
                    -> m(ChangeResourceRecordSets)
createChangeRequest recordType domain content = do
  ZoneId zoneId <- Config.get
  let
    recordSet = resourceRecordSet domain recordType
    updated   = addValue recordSet content
    changes   = changeBatch $ toNonEmpty [change Upsert updated]
    zone      = ResourceId zoneId
  return $ changeResourceRecordSets zone changes

addValue :: ResourceRecordSet -> Text -> ResourceRecordSet
addValue recordSet value = recordSet & rrsTTL ?~ 300
                                     & rrsResourceRecords ?~ pure(resourceRecord value)

parseStatus :: Int -> Maybe ServerError
parseStatus status
  | status >= 500 = Just err500 { errBody = "Unkown AWS Error" }
  | status >= 400 = Just err404 { errBody = "Could not locate AWS Resource" }
  | otherwise = Nothing
