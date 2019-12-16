module Fission.AWS.Route53
  ( createChangeRequest
  , registerDomain
  ) where

import           Network.AWS
import           Network.AWS.Auth as AWS
import           Network.AWS.Prelude hiding (hash)
import           Network.AWS.Route53
import           Servant

import           Fission.Prelude
import           Fission.Internal.UTF8
import qualified Fission.Config    as Config
import           Fission.AWS.Types as AWS
import           Fission.AWS

import qualified Network.IPFS.Types as IPFS
import           Network.IPFS.Gateway.Types
import           Network.IPFS.CID.Types

registerDomain ::
  ( MonadRIO           cfg m
  , MonadUnliftIO          m
  , HasLogFunc         cfg
  , Has IPFS.Gateway   cfg
  , Has AWS.AccessKey  cfg
  , Has AWS.SecretKey  cfg
  , Has AWS.ZoneID     cfg
  , Has AWS.DomainName cfg
  , Has AWS.Route53MockEnabled cfg
  )
  => Text
  -> CID
  -> m (Either ServerError AWS.DomainName)
registerDomain username (CID hash) = do
  gateway :: IPFS.Gateway   <- Config.get
  domain  :: AWS.DomainName <- Config.get

  let
    baseUrl    = username <> "."<> AWS.getDomainName domain
    dnslinkUrl = "_dnslink." <> baseUrl
    dnslink    = "dnslink=/ipfs/" <> hash

  changeRecord Cname baseUrl (getGateway gateway) >>= \case
    Left err -> return <| Left err
    Right _ ->
      changeRecord Txt dnslinkUrl (dnslink `wrapIn` "\"") >>= \case
        Left err -> return <| Left err
        Right _ -> return <| Right <| AWS.DomainName baseUrl

changeRecord ::
  ( MonadRIO            cfg m
  , MonadUnliftIO           m
  , HasLogFunc          cfg
  , Has AWS.AccessKey   cfg
  , Has AWS.SecretKey   cfg
  , Has AWS.ZoneID      cfg
  , Has AWS.Route53MockEnabled cfg
  )
  => RecordType
  -> Text
  -> Text
  -> m (Either ServerError ChangeResourceRecordSetsResponse)
changeRecord recordType domain content = do
  AWS.Route53MockEnabled mockRoute53 <- Config.get
  if mockRoute53
    then changeRecordMock recordType domain content
    else changeRecord' recordType domain content

-- | Mock changing the given DNS record on Route53
changeRecordMock :: (MonadRIO cfg m, HasLogFunc cfg)
  => RecordType
  -> Text
  -> Text
  -> m (Either ServerError ChangeResourceRecordSetsResponse)
changeRecordMock recordType domain content = do
    mockTime <- liftIO <| getCurrentTime

    let mockMessage = "MOCK: Updating DNS "
            <> displayShow recordType
            <> " record at: "
            <> displayShow domain
            <> " with "
            <> displayShow content
        mockId = "test123"
        mockChangeInfo = changeInfo mockId Pending mockTime
        mockRecordResponse = changeResourceRecordSetsResponse 300 mockChangeInfo

    logDebug mockMessage
    return (Right mockRecordResponse)

-- | Change the given DNS record on Route53
changeRecord' ::
  ( MonadRIO           cfg m
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
changeRecord' recordType domain content = do
  logDebug <| "Updating DNS record at: " <> displayShow domain

  env <- createEnv
  req <- createChangeRequest recordType domain content

  withAWS env NorthVirginia <| do
    res <- send req
    return <| validate res

-- | Create the AWS change request for Route53
createChangeRequest ::
  ( MonadRIO       cfg m
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
