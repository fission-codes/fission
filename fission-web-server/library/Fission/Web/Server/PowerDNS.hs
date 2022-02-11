module Fission.Web.Server.PowerDNS
  ( module Fission.Web.Server.PowerDNS.Class
  , getValuesFromRecords
  ) where

import           PowerDNS.Client                   as PDNS
import           RIO.NonEmpty                      (nonEmpty)

import           Fission.Prelude
import           Fission.Web.Server.PowerDNS.Class

getValuesFromRecords :: [SearchResult] -> Maybe (NonEmpty Text)
getValuesFromRecords rrs = nonEmpty (PDNS.sr_content <$> rrs)
