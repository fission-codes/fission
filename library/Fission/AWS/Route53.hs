module Fission.AWS.Route53
  ( changeRecord'
  , changeRecordMock
  , changeRecordMockPure
  , createHostedZone'
  , createHostedZoneMock
  , createHostedZoneMockPure
  , module Fission.AWS.Route53.Class
  ) where

import           Fission.Prelude
import           Servant

import           Data.List.NonEmpty as NonEmpty hiding ((<|))

import           Network.AWS         as AWS
import           Network.AWS.Route53 as Route53

import           Fission.AWS.Error
import           Fission.AWS.Types as AWS

import           Fission.AWS.Route53.Class

import qualified Fission.URL as URL

changeRecord' ::
  ( MonadAWS    m
  , MonadLogger m
  )
  => RecordType
  -> URL.DomainName
  -> Text 
  -> ZoneID
  -> m (Either ServerError ChangeResourceRecordSetsResponse)
changeRecord' recordType (URL.DomainName domain) content (ZoneID zoneId) = do
  logDebug <| "Updating DNS record at: " <> displayShow domain

  req <- createChangeRequest

  AWS.within NorthVirginia do
    res <- send req
    return <| validate res

  where
    -- | Create the AWS change request for Route53
    createChangeRequest =
      content
        |> addValue (resourceRecordSet domain recordType)
        |> change Upsert
        |> return
        |> changeBatch
        |> changeResourceRecordSets (ResourceId zoneId)
        |> return

    addValue recordSet value =
      recordSet
        |> rrsTTL ?~ 10
        |> rrsResourceRecords ?~ pure (resourceRecord value)

changeRecordMock ::
  ( MonadTime   m
  , MonadLogger m
  )
  => RecordType
  -> URL.DomainName
  -> Text 
  -> m (Either ServerError ChangeResourceRecordSetsResponse)
changeRecordMock recordType domain content = do
  let
    mockMessage = mconcat
      [ "MOCK: Updating DNS "
      , show recordType
      , " record at: "
      , show domain
      , " with "
      , show content
      ]
  logDebug mockMessage

  mockTime <- currentTime

  return <| changeRecordMockPure' mockTime

changeRecordMockPure ::
     UTCTime
  -> RecordType
  -> URL.DomainName
  -> Text 
  -> Either ServerError ChangeResourceRecordSetsResponse
changeRecordMockPure mockTime _ _ _ = changeRecordMockPure' mockTime

changeRecordMockPure' :: UTCTime -> Either ServerError ChangeResourceRecordSetsResponse
changeRecordMockPure' mockTime = do
  let
    mockStatus         = 300
    mockId             = "test123"
    mockChangeInfo     = changeInfo mockId Pending mockTime
    mockRecordResponse = changeResourceRecordSetsResponse mockStatus mockChangeInfo

  Right mockRecordResponse


createHostedZone' :: 
  ( MonadAWS    m
  , MonadLogger m
  )
  => URL.DomainName
  -> m (Either ServerError CreateHostedZoneResponse)
createHostedZone' (URL.DomainName domain) = do
  logDebug <| "Updating DNS record at: " <> displayShow domain

  let
    req = createHostedZone domain "CallerReference"
  
  res <- send req
  return <| validate res


createHostedZoneMock :: 
  ( MonadTime   m
  , MonadLogger m
  )
  => URL.DomainName
  -> m (Either ServerError CreateHostedZoneResponse)
createHostedZoneMock domain = do
  logDebug <| "MOCK: Creating HostedZone for domain: " <> show domain

  mockTime <- currentTime
  return <| createHostedZoneMockPure mockTime domain

createHostedZoneMockPure :: 
     UTCTime
  -> URL.DomainName
  -> Either ServerError CreateHostedZoneResponse
createHostedZoneMockPure mockTime (URL.DomainName domain) = do
  let
    mockStatus     = 300
    mockId         = "test123"
    mockZone       = hostedZone mockId domain "MockCallerReference"
    mockChangeInfo = changeInfo mockId Pending mockTime
    mockDelegation = delegationSet <| NonEmpty.fromList ["mock_delegation"]
    mockLocation   = "mock_location"
    mockResponse   = createHostedZoneResponse mockStatus mockZone mockChangeInfo mockDelegation mockLocation

  Right mockResponse
