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
create userSecret = do
  now <- liftIO getCurrentTime

  uID <- insertWithPK Table.users
    [User def Regular True Nothing userSecret <@ now]

  logInfo $ "Inserted user " <> display uID
  return uID

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

  uID <- insertWithPK Table.users
    [User def Regular True (Just hConfId) userSecret <@ now]

  logInfo $ "Inserted user " <> display uID
  return uID
