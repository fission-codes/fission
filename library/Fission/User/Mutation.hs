module Fission.User.Mutation (MonadDBMutation(..)) where

import           Crypto.BCrypt
import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)
import           Database.Persist          ((=.), update)

import           Fission.Models
import           Fission.Prelude
import           Fission.Types

import qualified Fission.Platform.Heroku.Region.Types as Heroku

import           Fission.User.Role.Types
import qualified Fission.User.Mutation.Error as Error
import qualified Fission.User.Password.Types as User

hashPassword' :: MonadIO m => Text -> m (Either Error.Create Text)
hashPassword' password = do
  hashed <- liftIO <| hashPasswordUsingPolicy slowerBcryptHashingPolicy <| encodeUtf8 password
  return <| case hashed of
    Nothing           -> Left Error.FailedDigest
    Just secretDigest -> Right <| decodeUtf8Lenient secretDigest

class MonadDB m => MonadDBMutation m where
  createWithHeroku :: MonadDB m => UUID -> Heroku.Region -> Text -> Text -> m (Either Error.Create UserId)
  create :: MonadIO m => Text -> Text -> Maybe Text -> Maybe HerokuAddOnId -> UTCTime -> Transaction m (Either Error.Create UserId)
  updatePassword :: MonadTime m => UserId -> User.Password -> m (Either Error.Create User.Password)
  destroy :: MonadDB m => UserId -> Transaction m ()
  destroyHerokuAddon :: MonadDB m => UUID -> Transaction m ()

instance MonadDBMutation Fission where
  -- | Create a new, timestamped entry and heroku add-on
  createWithHeroku herokuUUID herokuRegion username password = runDBNow \now -> do
    addOnId <- insert HerokuAddOn
      { herokuAddOnUuid       = herokuUUID
      , herokuAddOnRegion     = Just herokuRegion
      , herokuAddOnInsertedAt = now
      , herokuAddOnModifiedAt = now
      }

    create username password Nothing (Just addOnId) now

  -- | Create a new, timestamped entry with optional heroku add-on
  create username password email herokuAddOnId now =
    hashPassword' password >>= \case
      Left err ->
        return (Left err)

      Right secretDigest -> do
        let newUserRecord = User
              { userUsername      = username
              , userEmail         = email
              , userRole          = Regular
              , userActive        = True
              , userHerokuAddOnId = herokuAddOnId
              , userSecretDigest  = secretDigest
              , userInsertedAt    = now
              , userModifiedAt    = now
              }

        insertUnique newUserRecord >>= \case
          Just userID ->
            return (Right userID)

          Nothing ->
            return (Left Error.AlreadyExists)

  updatePassword userId (User.Password password) =
    hashPassword' password >>= \case
      Left err ->
        return (Left err)

      Right secretDigest -> do
        runDBNow \now -> update userId
          [ UserSecretDigest =. secretDigest
          , UserModifiedAt   =. now
          ]

        return . Right <| User.Password password

  destroy userId =
    delete <| from \user -> where_ (user ^. UserId ==. val userId)

  destroyHerokuAddon uuid =
    delete <| from \herokuAddOn -> where_ (herokuAddOn ^. HerokuAddOnUuid ==. val uuid)
