module Fission.AWS.Route53
  ( createChangeRequest
  , registerDomain
  ) where

import Network.AWS
import Network.AWS.Auth as AWS
import Network.AWS.Prelude
import Network.AWS.Route53
import Servant

import           Fission.Prelude
import qualified Fission.Config    as Config
import           Fission.AWS.Types as AWS
import           Fission.AWS

registerDomain
  :: ( MonadRIO           cfg m
     , MonadUnliftIO          m
     , HasLogFunc         cfg
     , Has AWS.AccessKey  cfg
     , Has AWS.SecretKey  cfg
     , Has AWS.ZoneID     cfg
     )
  => RecordType
  -> Text
  -> Text
  -> m (Either ServerError ChangeResourceRecordSetsResponse)
registerDomain recordType domain content = do
  logDebug <| "Updating DNS record at: " <> displayShow domain
  env <- createEnv
  req <- createChangeRequest recordType domain content

  withAWS env NorthVirginia <| do
    res <- send req
    return <| validate res

createChangeRequest
  :: ( MonadRIO       cfg m
     , Has AWS.ZoneID cfg
     )
  => RecordType
  -> Text
  -> Text
  -> m ChangeResourceRecordSets
createChangeRequest recordType domain content = do
  ZoneID zoneId <- Config.get
  return <| changeResourceRecordSets (ResourceId zoneId) changes
  where
    recordSet = resourceRecordSet domain recordType
    updated   = addValue recordSet content
    changes   = [change Upsert updated]
                  |> toNonEmpty
                  |> changeBatch

addValue :: ResourceRecordSet -> Text -> ResourceRecordSet
addValue recordSet value =
  recordSet |> rrsTTL ?~ 10
            |> rrsResourceRecords ?~ pure (resourceRecord value)
