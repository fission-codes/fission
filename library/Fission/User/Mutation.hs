module Fission.User.Mutation
  ( create
  , createWithHeroku
  , updatePassword
  ) where

import           Crypto.BCrypt
import           Data.UUID (UUID)

import           Fission.Prelude
import           Fission.Timestamp as Timestamp

import           Fission.Platform.Heroku.AddOn.Types as Heroku
import qualified Fission.Platform.Heroku.Types as Heroku
import           Fission.Storage.Database.Query as Query

import           Fission.User.Role
import           Fission.User.Types as User
import qualified Fission.User.Mutation.Error as Error
import qualified Fission.User.Password.Types as User


-- CREATE


{-| Create a new timestamped user.
-}
create
  :: ( MonadRIO    cfg m
     , HasLogFunc cfg
     )
  => Text
  -> Text
  -> Maybe Text
  -> m (Either Error.Create (UserId))
create = createUser Nothing


{-| Create a new timestamped user along with a Heroku add-on.
-}
createWithHeroku
  :: ( MonadRIO      cfg m
     , HasLogFunc    cfg
     )
  => UUID
  -> Heroku.Region
  -> Text
  -> Text
  -> m (Either Error.Create (UserId))
createWithHeroku herokuUUID herokuRegion username password = do
  now <- liftIO getCurrentTime

  -- Create the Heroku add-on for the user
  addOnId <- Query.insert <| Heroku.AddOn
    { region = Just herokuRegion
    , uuid = herokuUUID

    --
    , insertedAt = now
    , modifiedAt = now
    }

  -- Create the user
  createUser (Just addOnId) username password Nothing



-- UPDATE


{-| Update the password of a user.
-}
updatePassword
  :: ( MonadRIO    cfg m
     , HasLogFunc  cfg
     )
  => UserId
  -> User.Password
  -> m (Either Error.Create User.Password)
updatePassword userId' (User.Password password) =
  hashUserPassword password >>= \case
    Left err ->
      return (Left err)

    Right secretDigest' -> do
      -- Update `secretDigest` in the database
      Query.update \user ->
        Query.set user [ User.secretDigest =. Query.val secretDigest ]
        Query.where_ (user ^. UserId ==. userId')

      -- Log
      logInfo ("Updated password for user " <> displayShow userId)

      -- Return password
      password
        |> User.Password
        |> Right
        |> return



-- ㊙️


createUser
  :: ( MonadRIO    cfg m
     , HasLogFunc  cfg
     )
  => Maybe Heroku.AddOnId
  -> Text
  -> Text
  -> Maybe Text
  -> m (Either Error.Create (UserId))
createUser herokuAddOnId username password email = do
  hashUserPassword password >>= \case
    Left err ->
      return (Left err)

    Right secretDigest -> do
      now <- liftIO getCurrentTime

      -- Add the user to the database
      let active = True
      let role = Regular
      let insertedAt = now
      let modifiedAt = now

      let user = User { .. }

      userId <- Query.insert user

      -- Log
      logInfo ("Inserted user " <> displayShow userId)

      -- Return the id of the new user
      return (Right userId)


hashUserPassword :: MonadIO m => Text -> m (Either Error.Create Text)
hashUserPassword password = do
  hashed <- password
    |> encodeUtf8
    |> hashPasswordUsingPolicy slowerBcryptHashingPolicy
    |> liftIO

  return <| case hashed of
    Nothing           -> Left Error.FailedDigest
    Just secretDigest -> Right (decodeUtf8Lenient secretDigest)
