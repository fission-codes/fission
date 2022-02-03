module Fission.Web.Server.PowerDNS.Class (MonadPowerDNS(..)) where

import           PowerDNS.Client as PDNS
import           Servant

import           Fission.Prelude

import           Fission.URL     (URL)

class Monad m => MonadPowerDNS m where
  set ::
       RecordType
    -> URL
    -> Text -- ZoneID
    -> NonEmpty Text -- contents
    -> Word32
    -> m (Either ServerError NoContent)
