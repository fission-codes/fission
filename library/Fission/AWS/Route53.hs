module Fission.AWS.Route53
  ( changeRecord'
  , changeRecordMock
  , module Fission.AWS.Route53.Class
  ) where

import           Fission.Prelude
import           Servant

import           Network.AWS         as AWS
import           Network.AWS.Route53 as Route53

import           Fission.AWS.Error
import           Fission.AWS.Types as AWS

import           Fission.AWS.Route53.Class

import qualified Fission.URL as URL


changeRecordMock ::
  ( MonadTime m
  , MonadLogger m
  )
  => RecordType
  -> URL.DomainName
  -> Text 
  -> m (Either ServerError ChangeResourceRecordSetsResponse)
changeRecordMock recordType (URL.DomainName domain) content = do
  mockTime <- currentTime

  let
    mockMessage = mconcat
      [ "MOCK: Updating DNS "
      , show recordType
      , " record at: "
      , show domain
      , " with "
      , show content
      ]

    mockId             = "test123"
    mockChangeInfo     = changeInfo mockId Pending mockTime
    mockRecordResponse = changeResourceRecordSetsResponse 300 mockChangeInfo

  logDebug mockMessage
  return (Right mockRecordResponse)

changeRecord' ::
  ( MonadAWS m
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
