module Fission.AWS.Route53
  ( createChangeRequest
  , registerDomain
  ) where

import RIO
import Servant

import Control.Lens ((?~))
import Data.Has
import Servant

import Network.AWS
import Network.AWS.Auth as AWS
import Network.AWS.Prelude
import Network.AWS.Route53

import qualified Fission.Config    as Config
import           Fission.AWS.Types as AWS
import           Fission.AWS
import           Fission.Internal.Constraint

registerDomain :: MonadRIO           cfg m
               => MonadUnliftIO          m
               => HasLogFunc         cfg
               => Has AWS.AccessKey  cfg
               => Has AWS.SecretKey  cfg
               => Has AWS.ZoneId     cfg
               => RecordType
               -> Text
               -> Text
               -> m (Either ServerError ChangeResourceRecordSetsResponse)
registerDomain recordType domain content = do
  logDebug $ "Updating DNS record at: " <> displayShow domain
  env <- createEnv
  req <- createChangeRequest recordType domain content

  withAWS env NorthVirginia $ do
    res <- send req
    return $ validate res

createChangeRequest :: MonadRIO       cfg m
                    => Has AWS.ZoneId cfg
                    => RecordType
                    -> Text
                    -> Text
                    -> m ChangeResourceRecordSets
createChangeRequest recordType domain content = do
  ZoneId zoneId <- Config.get
  return $ changeResourceRecordSets (ResourceId zoneId) changes
  where
    recordSet = resourceRecordSet domain recordType
    updated   = addValue recordSet content
    changes   = changeBatch $ toNonEmpty [change Upsert updated]

addValue :: ResourceRecordSet -> Text -> ResourceRecordSet
addValue recordSet value =
  recordSet & rrsTTL ?~ 300
            & rrsResourceRecords ?~ pure (resourceRecord value)
