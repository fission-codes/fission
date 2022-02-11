module Fission.Web.Server.PowerDNS
  ( module Fission.Web.Server.PowerDNS.Class
  , getValuesFromRecords
  ) where

import           Fission.Prelude
import           Fission.Web.Server.PowerDNS.Class
import           PowerDNS.Client                   as PDNS

getValuesFromRecords :: [SearchResult] -> Maybe (NonEmpty Text)
getValuesFromRecords rrs = nonEmpty (PDNS.sr_content <$> rrs)
