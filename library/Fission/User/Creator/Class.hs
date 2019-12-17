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
import           Fission.User.DID           as DID
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
    -> Either DID Text
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
  create username didOrPW email herokuAddOnId now =
    parsePW didOrPW >>= \case
      Left err ->
        return <| Left <| openUnionLift err

      Right maybeDigest -> do
        let 
          maybeDid = parseDID didOrPW
          newUserRecord = User
              { userDid           = maybeDid
              , userUsername      = username
              , userEmail         = email
              , userRole          = Regular
              , userActive        = True
              , userHerokuAddOnId = herokuAddOnId
              , userSecretDigest  = maybeDigest
              , userInsertedAt    = now
              , userModifiedAt    = now
              }

        insertUnique newUserRecord >>= \case
          Just userID ->
            return (Right userID)

          Nothing ->
            return <| Left <| openUnionLift User.AlreadyExists
    where
      -- parsePW :: MonadIO m => Either DID Text -> m (Either Password.FailedDigest (Maybe Text))
      parsePW (Left _) = return <| Right Nothing
      parsePW (Right pw) = Password.hashPassword pw >>= \case
        Left err -> return <| Left err
        Right digest -> return <| Right <| Just digest

      parseDID :: Either DID Text -> Maybe Text
      parseDID (Left did) = Just <| unDID did
      parseDID (Right _) = Nothing

  createWithHeroku herokuUUID herokuRegion username password now = do
    Heroku.AddOn.create herokuUUID herokuRegion now >>= \case
      Right addOnId -> create username (Right password) Nothing (Just addOnId) now
      Left  err     -> return <| Left <| openUnionLift err
