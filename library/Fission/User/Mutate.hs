module Fission.User.Mutate
  (
    create,
    createWithHeroku
  ) where

import           RIO

import Database.Selda

import Data.Time (getCurrentTime)
import Data.UUID (UUID)

import           Fission.Internal.Constraint
import           Fission.Internal.Orphanage.ID ()

import           Fission.Security.Types (SecretDigest)
import           Fission.Timestamp as Timestamp

import qualified Fission.Platform.Heroku.AddOn as Heroku
import qualified Fission.Platform.Heroku.Types as Heroku

import           Fission.User.Role
import           Fission.User.Types
import qualified Fission.User.Table as Table

-- | Create a new, timestamped entry
create :: MonadRIO    cfg m
       => MonadSelda      m
       => HasLogFunc cfg
       => SecretDigest
       -> m (ID User)
create userSecret = create' userSecret Nothing

-- | Create a new, timestamped entry and heroku add-on
createWithHeroku :: MonadRIO    cfg m
       => MonadSelda      m
       => HasLogFunc cfg
       => UUID
       -> Heroku.Region
       -> SecretDigest
       -> m (ID User)
createWithHeroku herokuUUID herokuRegion userSecret = do
  now <- liftIO getCurrentTime

  hConfId <- insertWithPK Heroku.addOns
    [Heroku.AddOn def herokuUUID (Just herokuRegion) <@ now]
  
  create' userSecret (Just hConfId)

-- | Create a new, timestamped entry with optional heroku add-on
create' :: MonadRIO    cfg m
       => MonadSelda      m
       => HasLogFunc cfg
       => SecretDigest
       -> Maybe (ID Heroku.AddOn)
       -> m (ID User)
create' userSecret herokuUUID = do
  now <- liftIO getCurrentTime

  uID <- insertWithPK Table.users
    [User def Regular True herokuUUID userSecret <@ now]

  logInfo $ "Inserted user " <> display uID
  return uID
