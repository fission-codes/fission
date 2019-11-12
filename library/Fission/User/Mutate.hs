module Fission.User.Mutate
  ( create
  , createWithHeroku
  , updatePassword
  ) where

import           RIO

import Database.Selda

import Data.Time (getCurrentTime)
import Data.UUID (UUID)

import Crypto.BCrypt

import           Fission.Internal.Constraint
import           Fission.Internal.Orphanage.ID ()

import           Fission.Timestamp as Timestamp

import qualified Fission.Platform.Heroku.AddOn as Heroku
import qualified Fission.Platform.Heroku.Types as Heroku

import           Fission.User.Role
import           Fission.User.Types
import qualified Fission.User.Table          as Table
import qualified Fission.User.Mutate.Error   as Error
import qualified Fission.User.Password.Types as User

-- | Create a new, timestamped entry
create :: MonadRIO    cfg m
       => MonadSelda      m
       => HasLogFunc cfg
       => Text
       -> Text
       -> Maybe Text
       -> m (Either Error.Create (ID User))
create username password email = create' username password email Nothing

-- | Create a new, timestamped entry and heroku add-on
createWithHeroku :: MonadRIO    cfg m
                 => MonadSelda      m
                 => HasLogFunc cfg
                 => UUID
                 -> Heroku.Region
                 -> Text
                 -> Text
                 -> m (Either Error.Create (ID User))
createWithHeroku herokuUUID herokuRegion username password = do
  now <- liftIO getCurrentTime

  hConfId <- insertWithPK Heroku.addOns
    [Heroku.AddOn def herokuUUID (Just herokuRegion) <@ now]

  create' username password Nothing (Just hConfId)

-- | Create a new, timestamped entry with optional heroku add-on
create' :: MonadRIO    cfg m
        => MonadSelda      m
        => HasLogFunc  cfg
        => Text
        -> Text
        -> Maybe Text
        -> Maybe (ID Heroku.AddOn)
        -> m (Either Error.Create (ID User))
create' username password email herokuUUID = do
  now <- liftIO getCurrentTime
  hashPassword' password >>= \case
    Left err -> return $ Left err
    Right secretDigest -> do
      uID <- insertWithPK Table.users
        [User def username email Regular True herokuUUID secretDigest <@ now]
      logInfo $ "Inserted user " <> display uID
      return $ Right uID

updatePassword :: MonadRIO    cfg m
               => MonadSelda      m
               => HasLogFunc  cfg
               => ID User
               -> User.Password
               -> m (Either Error.Create User.Password)
updatePassword userID (User.Password password) = do
  hashPassword' password >>= \case
    Left err -> return $ Left err
    Right secretDigest -> do
      update Table.users
        (\user -> user ! #_userID .== literal userID)
        (\user -> user `with` [#_secretDigest := literal secretDigest])

      logInfo $ "Updated password for user " <> display userID
      return . Right $ User.Password password

hashPassword' :: MonadIO m
             => Text
             -> m (Either Error.Create Text)
hashPassword' password = do
  bar <- liftIO (hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 password))
  case bar of
    Nothing -> return $ Left Error.FailedDigest
    Just secretDigest -> return . Right $ decodeUtf8Lenient secretDigest
