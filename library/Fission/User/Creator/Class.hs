module Fission.User.Creator.Class
  ( Creator (..)
  , Errors
  ) where

import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((<&>))
import           Servant

import           Fission.Prelude
import           Fission.Error as Error
import           Fission.Models

import           Fission.Key as Key
import           Fission.URL

import qualified Fission.Platform.Heroku.Region.Types  as Heroku
import qualified Fission.Platform.Heroku.AddOn.Creator as Heroku.AddOn

import qualified Fission.User.Creator.Error as User
import           Fission.User.Password      as Password
import           Fission.User.Types
import qualified Fission.User.Username      as Username
import qualified Fission.User.Validation    as User

import qualified Fission.App.Domain  as App.Domain
import qualified Fission.App.Content as App.Content

import qualified Fission.User.Modifier as User
import qualified Fission.App.Creator   as App

type Errors = OpenUnion
  '[ ActionNotAuthorized App
   , NotFound            App

   , ActionNotAuthorized URL
   , NotFound            URL

   , AlreadyExists HerokuAddOn
   , App.Domain.AlreadyAssociated
  
   , User.AlreadyExists
   , NotFound User
  
   , Username.Invalid
   , Password.FailedDigest

   , InvalidURL

   , ServerError
   ]

class Heroku.AddOn.Creator m => Creator m where
  -- | Create a new, timestamped entry
  create ::
       Username
    -> Key.Public
    -> Email
    -> UTCTime
    -> m (Either Errors UserId)

  createWithPassword ::
       Username
    -> Password
    -> Email
    -> UTCTime
    -> m (Either Errors UserId)

  -- | Create a new, timestamped entry and heroku add-on
  createWithHeroku ::
       UUID
    -> Heroku.Region
    -> Username
    -> Password
    -> UTCTime
    -> m (Either Errors UserId)

instance
  ( MonadIO                 m
  , App.Domain.Initializer  m
  , App.Content.Initializer m
  )
  => Creator (Transaction m) where
  create username pk email now =
    User
      { userPublicKey     = Just pk
      , userUsername      = username
      , userEmail         = Just email
      , userRole          = Regular
      , userActive        = True
      , userVerified      = False
      , userHerokuAddOnId = Nothing
      , userSecretDigest  = Nothing
      , userDataRoot      = App.Content.empty
      , userInsertedAt    = now
      , userModifiedAt    = now
      }
      |> User.check
      |> \case
        Left err ->
          return $ Error.openLeft err

        Right user ->
          insertUnique user >>= \case
            Nothing ->
              determineConflict username (Just pk)

            Just userId ->
              User.setData userId App.Content.empty now >>= \case
                Left err ->
                  return $ Error.relaxedLeft err

                Right () ->
                  App.createWithPlaceholder userId now <&> \case
                    Left err -> Error.relaxedLeft err
                    Right _  -> Right userId

  createWithPassword username password email now =
    Password.hashPassword password >>= \case
      Left err ->
        return $ Error.openLeft err

      Right secretDigest ->
        User
          { userPublicKey     = Nothing
          , userUsername      = username
          , userEmail         = Just email
          , userRole          = Regular
          , userActive        = True
          , userVerified      = False
          , userHerokuAddOnId = Nothing
          , userSecretDigest  = Just secretDigest
          , userDataRoot      = App.Content.empty
          , userInsertedAt    = now
          , userModifiedAt    = now
          }
          |> insertUnique
          |> bind \case
            Nothing ->
              determineConflict username Nothing

            Just userId ->
              App.createWithPlaceholder userId now <&> \case
                Left err -> Error.relaxedLeft err
                Right _  -> Right userId

  createWithHeroku herokuUUID herokuRegion username password now =
    Heroku.AddOn.create herokuUUID herokuRegion now >>= \case
      Left err ->
        return $ Error.openLeft err

      Right herokuAddOnId ->
        Password.hashPassword password >>= \case
          Left err ->
            return $ Error.openLeft err

          Right secretDigest ->
            User
              { userPublicKey     = Nothing
              , userUsername      = username
              , userEmail         = Nothing
              , userRole          = Regular
              , userActive        = True
              , userVerified      = True
              , userHerokuAddOnId = Just herokuAddOnId
              , userSecretDigest  = Just secretDigest
              , userDataRoot      = App.Content.empty
              , userInsertedAt    = now
              , userModifiedAt    = now
              }
              |> insertUnique
              |> bind \case
                Just userID -> return $ Right userID
                Nothing     -> determineConflict username Nothing

determineConflict ::
  MonadIO m
  => Username
  -> Maybe Key.Public
  -> Transaction m (Either Errors a)
 
determineConflict username Nothing =
  return . Error.openLeft $ User.ConflictingUsername username
 
determineConflict username (Just pk) = do
  -- NOTE needs to be updated anlong with DB constraints
  --      because Postgres doesn't do this out of the box

  conflUN <- getBy (UniqueUsername username) <&> fmap \_ ->
    User.ConflictingUsername username

  conflPK <- getBy (UniquePublicKey $ Just pk) <&> fmap \_ ->
    User.ConflictingPublicKey pk

  -- conflEmail TODO

  return case conflUN <|> conflPK of
    Just err -> Error.openLeft err
    Nothing  -> Error.openLeft err409 { errBody = "User already exists" }
