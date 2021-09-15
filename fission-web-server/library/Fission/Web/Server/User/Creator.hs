module Fission.Web.Server.User.Creator
  ( module Fission.Web.Server.User.Creator.Class
  , module Fission.Web.Server.User.Creator.Error
  , createDB
  , createWithPasswordDB
  , createWithHerokuDB
  ) where


import           Data.UUID                               (UUID)
import           Database.Esqueleto                      hiding ((<&>))

import           Network.IPFS.Bytes.Types
import           Servant

import           Fission.Prelude

import           Fission.Error                           as Error
import           Fission.Key                             as Key
import           Fission.User.DID.Types                  as DID
import           Fission.User.Email.Types

import qualified Fission.Platform.Heroku.Region.Types    as Heroku

import qualified Fission.Web.Server.App.Content          as App.Content
import qualified Fission.Web.Server.Heroku.AddOn.Creator as Heroku.AddOn

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB

import           Fission.Web.Server.User.Creator.Class
import           Fission.Web.Server.User.Creator.Error
import qualified Fission.Web.Server.User.Creator.Error   as User
import           Fission.Web.Server.User.Password        as Password
import           Fission.Web.Server.User.Types

createDB ::
     MonadIO m
  => Username
  -> Key.Public
  -> Email
  -> UTCTime
  -> Transaction m (Either Errors' UserId)
createDB username did email now =
  insertUnique user >>= \case
    Just userId -> return $ Right userId
    Nothing     -> determineConflict username (Just pk)
  where
    user =
      User
        { userPublicKey
        , userIonId
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

    (userPublicKey, userIonId) =
      case did of
        DID.Key pk  -> (Just pk, Nothing)
        DID.Ion txt -> (Nothing, Just txt)

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
          Nothing     -> determineConflict username Nothing

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
