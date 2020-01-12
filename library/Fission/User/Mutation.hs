module Fission.User.Mutation (Insertable (..), Mutable (..)) where

import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)
import           Database.Persist          ((=.), update)

import           Fission.Models
import           Fission.Prelude

import qualified Fission.Platform.Heroku.Region.Types as Heroku

import           Fission.User.Role.Types
import qualified Fission.User.Mutation.Error as Error
import qualified Fission.User.Password.Types as User
import qualified Fission.User.Password as Password

class Insertable m where
  -- | Create a new, timestamped entry with optional heroku add-on
  create :: Text -> Text -> Maybe Text -> Maybe HerokuAddOnId -> UTCTime -> m (Either Error.Create UserId)

  -- | Create a new, timestamped entry and heroku add-on
  createWithHeroku :: UUID -> Heroku.Region -> Text -> Text -> UTCTime -> m (Either Error.Create UserId)

class Mutable m where
  updatePassword     :: UserId -> User.Password -> UTCTime -> m (Either Error.Create User.Password)
  destroy            :: UserId  -> m ()
  destroyHerokuAddon :: UUID    -> m ()

instance MonadIO m => Insertable (Transaction m) where
  create ::
       Text
    -> Text
    -> Maybe Text
    -> Maybe HerokuAddOnId
    -> UTCTime
    -> Transaction m (Either Error.Create UserId)
  create username password email herokuAddOnId now =
    Password.hashPassword password >>= \case
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

        Transaction do
          insertUnique newUserRecord >>= \case
            Just userID ->
              return (Right userID)

            Nothing ->
              return (Left Error.AlreadyExists)

  createWithHeroku ::
       UUID
    -> Heroku.Region
    -> Text
    -> Text
    -> UTCTime
    -> Transaction m (Either Error.Create UserId)
  createWithHeroku herokuUUID herokuRegion username password now = do
    addOnId <- Transaction <| insert HerokuAddOn -- TODO EXTRACT INTO A TYPECLASS
      { herokuAddOnUuid       = herokuUUID
      , herokuAddOnRegion     = Just herokuRegion
      , herokuAddOnInsertedAt = now
      , herokuAddOnModifiedAt = now
      }

    create username password Nothing (Just addOnId) now

instance MonadIO m => Mutable (Transaction m) where
  updatePassword :: UserId -> User.Password -> UTCTime -> Transaction m (Either Error.Create User.Password)
  updatePassword userId (User.Password password) now = Transaction do
    Password.hashPassword password >>= \case
      Left err ->
        return (Left err)

      Right secretDigest -> do
        update userId
          [ UserSecretDigest =. secretDigest
          , UserModifiedAt   =. now
          ]

        return . Right <| User.Password password

  destroy :: UserId -> Transaction m ()
  destroy userId = Transaction <| delete <| from \user ->
    where_ (user ^. UserId ==. val userId)

  destroyHerokuAddon :: UUID -> Transaction m ()
  destroyHerokuAddon uuid = Transaction <| delete <| from \herokuAddOn ->
    where_ (herokuAddOn ^. HerokuAddOnUuid ==. val uuid)
