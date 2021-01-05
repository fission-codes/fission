module Fission.Web.Server.AWS.Route53
  ( module Fission.Web.Server.AWS.Route53.Class
  , verifyFirstResource
  , getValuesFromRecords
  )
where

import           Network.AWS.Route53
import           RIO

import           Fission.Prelude

import           Fission.Web.Server.AWS.Route53.Class

verifyFirstResource :: ListResourceRecordSetsResponse -> Text -> Maybe ResourceRecordSet
verifyFirstResource resp name =
  case headMaybe recordSets of
    Nothing  -> Nothing
    Just rrs -> if (rrs ^. rrsName) == (name <> ".") then Just rrs else Nothing
  where recordSets = resp ^. lrrsrsResourceRecordSets

getValuesFromRecords :: ResourceRecordSet -> Maybe (NonEmpty Text)
getValuesFromRecords rrs = case maybeRecords of
  Nothing      -> Nothing
  Just records -> Just $ (^. rrValue) <$> records
  where maybeRecords = rrs ^. rrsResourceRecords
