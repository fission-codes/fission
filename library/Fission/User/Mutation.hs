module Fission.User.Mutation
  ( create
  , createWithHeroku
  , updatePassword
  , destroy
  , destroyHerokuAddon
  ) where

import           Crypto.BCrypt
import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)
import           Database.Persist          ((=.), update)

import           Fission.Models
import           Fission.Prelude

import qualified Fission.Platform.Heroku.Region.Types as Heroku

import           Fission.User.Role.Types
import qualified Fission.User.Mutation.Error as Error
import qualified Fission.User.Password.Types as User

-- | Create a new, timestamped entry and heroku add-on
createWithHeroku ::
  ( MonadDB     m
  , MonadTime   m
  )
  => UUID
  -> Heroku.Region
  -> Text
  -> Text
  -> m (Either Error.Create UserId)
createWithHeroku herokuUUID herokuRegion username password = runDBNow \now -> do
  let herokuAddOnRecord = HerokuAddOn
        { herokuAddOnUuid       = herokuUUID
        , herokuAddOnRegion     = Just herokuRegion
        , herokuAddOnInsertedAt = now
        , herokuAddOnModifiedAt = now
        }
  insertUnique herokuAddOnRecord >>= \case
    Just addOnId ->
      create username password Nothing (Just addOnId) now

    Nothing ->
      return (Left Error.AlreadyExists)

-- | Create a new, timestamped entry with optional heroku add-on
create ::
  MonadIO m
  => Text
  -> Text
  -> Maybe Text
  -> Maybe HerokuAddOnId
  -> UTCTime
  -> Transaction m (Either Error.Create UserId)
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

updatePassword ::
  ( MonadDB     m
  , MonadTime   m
  )
  => UserId
  -> User.Password
  -> m (Either Error.Create User.Password)
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

hashPassword' :: MonadIO m => Text -> m (Either Error.Create Text)
hashPassword' password = do
  hashed <- liftIO <| hashPasswordUsingPolicy slowerBcryptHashingPolicy <| encodeUtf8 password
  return <| case hashed of
    Nothing           -> Left Error.FailedDigest
    Just secretDigest -> Right <| decodeUtf8Lenient secretDigest

destroy :: MonadDB m => UserId -> Transaction m ()
destroy userId =
  delete <| from \user -> where_ (user ^. UserId ==. val userId)

destroyHerokuAddon :: MonadDB m => UUID -> Transaction m ()
destroyHerokuAddon uuid =
  delete <| from \herokuAddOn -> where_ (herokuAddOn ^. HerokuAddOnUuid ==. val uuid)
