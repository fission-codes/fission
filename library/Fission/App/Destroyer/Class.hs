module Fission.App.Destroyer.Class
  ( Destroyer (..)
  , Errors
  ) where

import           Database.Esqueleto
-- import qualified Database.Persist as P

import           Fission.Prelude
import           Fission.Models
-- import           Fission.URL

-- import qualified Fission.Error as Error

import qualified Fission.App.Domain.Dissociate.Error as DissociateDomain

type Errors = OpenUnion
  '[ DissociateDomain.NotRegisteredToApp
   ]

class Monad m => Destroyer m where
  destroy :: AppId -> UTCTime -> m (Either Errors ())

instance MonadIO m => Destroyer (Transaction m) where
  destroy appId now = do
    appDomains <- select <| from \appDomain -> do
      where_ <| appDomain ^. AppDomainAppId ==. val appId
      return appDomain

    putMany (toEvent <$> appDomains)
    deleteCascade appId

    return ok
    where
      toEvent :: Entity AppDomain -> DissociateAppDomainEvent
      toEvent (Entity _ AppDomain {..}) =
        DissociateAppDomainEvent
          { dissociateAppDomainEventAppId      = appDomainAppId
          , dissociateAppDomainEventDomainName = appDomainDomainName
          , dissociateAppDomainEventSubdomain  = appDomainSubdomain
          , dissociateAppDomainEventInsertedAt = now
          }
