module Fission.Domain.Creator.Class (Creator (..)) where

import qualified Database.Persist             as Persist

import           Fission.Models
import           Fission.Prelude

import           Fission.AWS.Zone.Types
import           Fission.URL.DomainName.Types

-- | Domain registration / creation
class Monad m => Creator m where
  -- | Register a new domain name
  create ::
       DomainName -- ^ The @DomainName@ to register
    -> UserId     -- ^ Who is registering it
    -> ZoneID     -- ^ The associated AWS hosted zone ID
    -> UTCTime    -- ^ The time this is being performed ("now")
    -> m ()

instance MonadIO m => Creator (Transaction m) where
  create domainName ownerId zoneId now =
    Persist.insert_ $ Domain domainName ownerId zoneId now now
