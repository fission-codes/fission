module Fission.User.Creator.Class (Creator (..)) where

import           Data.UUID (UUID)
import           Database.Esqueleto

import           Fission.Models
import           Fission.Prelude

import qualified Fission.Platform.Heroku.Region.Types as Heroku
import qualified Fission.Platform.Heroku.AddOn.Creator as Heroku.AddOn

import           Fission.User.Role.Types
import           Fission.User.Creator.Error
import qualified Fission.User.Password as Password

class Heroku.AddOn.Creator m => Creator m where
  -- | Create a new, timestamped entry with optional heroku add-on
  create :: Text -> Text -> Maybe Text -> Maybe HerokuAddOnId -> UTCTime -> m (Either Error UserId)

  -- | Create a new, timestamped entry and heroku add-on
  createWithHeroku :: UUID -> Heroku.Region -> Text -> Text -> UTCTime -> m (Either Error UserId)

instance MonadIO m => Creator (Transaction m) where
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

        insertUnique newUserRecord >>= \case
          Just userID ->
            return (Right userID)

          Nothing ->
            return (Left AlreadyExists)

  createWithHeroku herokuUUID herokuRegion username password now = do
    Heroku.AddOn.create herokuUUID herokuRegion now >>= \case
      Right addOnId -> create username password Nothing (Just addOnId) now
      -- Left  err     -> return (Left err) -- FIXME! Need an open union
      Left  _err     -> return (Left AlreadyExists) -- FIXME! Need an open union
