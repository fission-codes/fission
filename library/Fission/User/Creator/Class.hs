module Fission.User.Creator.Class
  ( Creator (..)
  , Errors
  ) where

import           Data.UUID (UUID)
import           Database.Esqueleto

import           Fission.Models
import           Fission.Prelude

import qualified Fission.Platform.Heroku.Region.Types  as Heroku
import qualified Fission.Platform.Heroku.AddOn.Creator as Heroku.AddOn

import qualified Fission.User.Creator.Error as User
import qualified Fission.User.Password      as Password
import           Fission.User.Role.Types

type Errors = OpenUnion
  '[ User.AlreadyExists
   , Heroku.AddOn.Error
   , Password.FailedDigest
   ]

class Heroku.AddOn.Creator m => Creator m where
  -- | Create a new, timestamped entry with optional heroku add-on
  create ::
       Text
    -> Text
    -> Maybe Text
    -> Maybe HerokuAddOnId
    -> UTCTime
    -> m (Either Errors UserId)

  -- | Create a new, timestamped entry and heroku add-on
  createWithHeroku ::
       UUID
    -> Heroku.Region
    -> Text
    -> Text
    -> UTCTime
    -> m (Either Errors UserId)

instance MonadIO m => Creator (Transaction m) where
  create username password email herokuAddOnId now =
    Password.hashPassword password >>= \case
      Left err ->
        return <| Left <| openUnionLift err

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
            return <| Left <| openUnionLift User.AlreadyExists

  createWithHeroku herokuUUID herokuRegion username password now = do
    Heroku.AddOn.create herokuUUID herokuRegion now >>= \case
      Right addOnId -> create username password Nothing (Just addOnId) now
      Left  err     -> return <| Left <| openUnionLift err
