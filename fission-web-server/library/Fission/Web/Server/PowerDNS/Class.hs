module Fission.Web.Server.PowerDNS.Class (MonadPowerDNS(..)) where

import           Fission.Prelude
import           PowerDNS.Client as PDNS
import           Servant

import           Fission.URL     (URL)

class Monad m => MonadPowerDNS m where
  set ::
       RecordType
    -> URL
    -> Text -- ^ ZoneID
    -> NonEmpty Text -- ^ Contents
    -> Word32
    -> m (Either ServerError NoContent)

  get ::
       URL
    -> Text -- ^ ZoneID
    -> m (Either ServerError [SearchResult])

  clear ::
       URL
    -> Text -- ^ ZoneID
    -> m (Either ServerError ())

