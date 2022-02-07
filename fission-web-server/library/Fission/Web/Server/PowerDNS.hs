module Fission.Web.Server.PowerDNS
  ( module Fission.Web.Server.PowerDNS.Class
  , getValuesFromRecords) where

import           Fission.Prelude
import           Fission.Web.Server.PowerDNS.Class

import           Data.List                         (head)
import           PowerDNS.Client                   as PDNS

getValuesFromRecords :: [SearchResult] -> Maybe (NonEmpty Text)
getValuesFromRecords rrs = case rrs of
  []     -> Nothing
  result -> Just $ pure (PDNS.sr_content $ head result)
