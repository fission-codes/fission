module Fission.User.Creator
  ( module Fission.User.Creator.Class
  , module Fission.User.Creator.Error
  , createDB
  , createWithPasswordDB
  , createWithHerokuDB
  ) where

import           Fission.User.Creator.Class
import           Fission.User.Creator.Error

import           Data.UUID                             (UUID)
import           Database.Esqueleto                    hiding ((<&>))
import           Servant

import           Fission.Error                         as Error
import           Fission.Models
import           Fission.Prelude

import           Fission.Key                           as Key

import qualified Fission.Platform.Heroku.AddOn.Creator as Heroku.AddOn
import qualified Fission.Platform.Heroku.Region.Types  as Heroku

import qualified Fission.User.Creator.Error            as User
import           Fission.User.Password                 as Password
import           Fission.User.Types
import qualified Fission.User.Validation               as User

import           Network.IPFS.Bytes.Types

import qualified Fission.App.Content                   as App.Content


createDB ::
     MonadIO m
  => Username
  -> Key.Public
  -> Email
  -> UTCTime
  -> Transaction m (Either Errors' UserId)
createDB username pk email now =
  User
    { userPublicKey     = Just pk
    , userExchangeKeys  = Just []
    , userUsername      = username
    , userEmail         = Just email
    , userRole          = Regular
    , userActive        = True
    , userVerified      = False
    , userHerokuAddOnId = Nothing
    , userSecretDigest  = Nothing
    , userDataRoot      = App.Content.empty
    , userDataRootSize  = Bytes 0
    , userInsertedAt    = now
    , userModifiedAt    = now
    }
    |> User.check
    |> \case
      Left err ->
        return $ Error.openLeft err

      Right user ->
        insertUnique user >>= \case
          Just userId -> return $ Right userId
          Nothing -> determineConflict username (Just pk)

createWithPasswordDB ::
     MonadIO m
  => Username
  -> Password
  -> Email
  -> UTCTime
  -> Transaction m (Either Errors' UserId)
createWithPasswordDB username password email now =
  Password.hashPassword password >>= \case
    Left err ->
      return $ Error.openLeft err

    Right secretDigest ->
      User
        { userPublicKey     = Nothing
        , userExchangeKeys  = Just []
        , userUsername      = username
        , userEmail         = Just email
        , userRole          = Regular
        , userActive        = True
        , userVerified      = False
        , userHerokuAddOnId = Nothing
        , userSecretDigest  = Just secretDigest
        , userDataRoot      = App.Content.empty
        , userDataRootSize  = Bytes 0
        , userInsertedAt    = now
        , userModifiedAt    = now
        }
        |> insertUnique
        |> bind \case
          Just userId -> return $ Right userId
          Nothing -> determineConflict username Nothing

createWithHerokuDB ::
     MonadIO m
  => UUID
  -> Heroku.Region
  -> Username
  -> Password
  -> UTCTime
  -> Transaction m (Either Errors' UserId)
createWithHerokuDB herokuUUID herokuRegion username password now =
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
            , userExchangeKeys  = Just []
            , userUsername      = username
            , userEmail         = Nothing
            , userRole          = Regular
            , userActive        = True
            , userVerified      = True
            , userHerokuAddOnId = Just herokuAddOnId
            , userSecretDigest  = Just secretDigest
            , userDataRoot      = App.Content.empty
            , userDataRootSize  = Bytes 0
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
  -> Transaction m (Either Errors' a)

determineConflict username Nothing =
  return . Error.openLeft $ User.ConflictingUsername username

determineConflict username (Just pk) = do
  -- NOTE needs to be updated along with DB constraints
  --      because Postgres doesn't do this out of the box

  conflUN <- getBy (UniqueUsername username) <&> fmap \_ ->
    User.ConflictingUsername username

  conflPK <- getBy (UniquePublicKey $ Just pk) <&> fmap \_ ->
    User.ConflictingPublicKey pk

  -- conflEmail TODO

  return case conflUN <|> conflPK of
    Just err -> Error.openLeft err
    Nothing  -> Error.openLeft err409 { errBody = "User already exists" }
