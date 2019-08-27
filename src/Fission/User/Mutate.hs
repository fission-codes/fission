module Fission.User.Mutate
  ( create
  , deactivate
  ) where

import           RIO

import Database.Selda

import Data.Time (getCurrentTime)
import Data.UUID (UUID)

import           Fission.Internal.Constraint
import           Fission.Internal.Orphanage ()

import           Fission.Security.Types (SecretDigest)
import           Fission.Timestamp as Timestamp

import qualified Fission.Platform.Heroku.AddOn as Heroku
import qualified Fission.Platform.Heroku.Types as Heroku

import           Fission.User.Role
import           Fission.User.Selector
import           Fission.User.Types
import qualified Fission.User.Table as Table

-- | Create a new, timestamped entry
create :: MonadRIO    cfg m
       => MonadSelda      m
       => HasLogFunc cfg
       => UUID
       -> Heroku.Region
       -> SecretDigest
       -> m (ID User)
create herokuUUID herokuRegion sekret = do
  now <- liftIO getCurrentTime

  hConfId <- insertWithPK Heroku.addOns
    [Heroku.AddOn def herokuUUID (Just herokuRegion) <@ now]

  uID <- insertWithPK Table.users
    [User def Regular True (Just hConfId) sekret <@ now]

  logInfo $ "Inserted user " <> display uID
  return uID

deactivate :: MonadRIO    cfg m
           => MonadSelda      m
           => HasLogFunc cfg
           => ID User
           -> m Bool
deactivate userID = do
  n <- update Table.users (userID' `is` userID) $
        \user -> user `with` [active' := false]

  logInfo $ "Deactivated user ID: " <> display userID
  return $ n > 0
