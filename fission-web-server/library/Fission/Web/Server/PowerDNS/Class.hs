module Fission.Web.Server.PowerDNS.Class (MonadPowerDNS(..)) where

import           Fission.Prelude
import           PowerDNS.Client as PDNS
import           Servant
import           Servant.Client

import           Fission.URL     (URL)

class Monad m => MonadPowerDNS m where
  getClientEnv :: m ClientEnv

  set ::
       RecordType
    -> URL
    -> Text -- ZoneID
    -> NonEmpty Text -- contents
    -> Word32
    -> m (Either ServerError NoContent)

  get ::
       URL
    -> Text -- ZoneID
    -> m (Either ServerError [SearchResult])

  clear ::
       URL
    -> Text -- ZoneID
    -> m (Either ServerError ())
