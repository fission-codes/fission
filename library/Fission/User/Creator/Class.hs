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

import qualified Fission.Platform.Heroku.Region.Types  as Heroku
import qualified Fission.Platform.Heroku.AddOn.Creator as Heroku.AddOn

import qualified Fission.User.Creator.Error as User
import           Fission.User.Modifier      as User
import           Fission.User.Password      as Password
import           Fission.User.Types
import qualified Fission.User.Username      as Username
import qualified Fission.User.Validation    as User

import qualified Fission.App.Domain  as AppDomain
import qualified Fission.App.Creator as App

import           Fission.IPFS.DNSLink
import           Fission.URL.Subdomain.Types

import qualified Fission.App.Content as App.Content
import qualified Fission.App.Domain  as App.Domain

type Errors = OpenUnion
  '[ ActionNotAuthorized App
   , NotFound            App

   , AlreadyExists HerokuAddOn
   , AppDomain.AlreadyAssociated
   , User.AlreadyExists
  
   , Username.Invalid
   , Password.FailedDigest

   , ServerError
   ]

class Heroku.AddOn.Creator m => Creator m where
  -- | Create a new, timestamped entry
  create ::
       Username
    -> Key.Public
    -> Key.Algorithm
    -> Email
    -> UTCTime
    -> m (Either Errors (UserId, Subdomain))

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
  , MonadDNSLink            m
  , App.Domain.Initializer  m
  , App.Content.Initializer m
  )
  => Creator (Transaction m) where
  create username pk algo email now =
    User
      { userPublicKey     = Just pk
      , userAlgorithm     = Just algo
      , userUsername      = username
      , userEmail         = Just email
      , userRole          = Regular
      , userActive        = True
      , userHerokuAddOnId = Nothing
      , userSecretDigest  = Nothing
      , userDataRoot      = App.Content.empty
      , userInsertedAt    = now
      , userModifiedAt    = now
      }
      |> User.check
      |> \case
        Left err ->
          return (Error.openLeft err)

        Right user ->
          insertUnique user >>= \case
            Nothing -> do
              -- NOTE needs to be updated anlong with DB constraints
              --      because Postgres doesn't do this out of the box

              -- FIXME stringly typed
              conflUN <- (fmap \_ -> "username")  <$> getBy (UniqueUsername username)
              conflPK <- (fmap \_ -> "publicKey") <$> getBy (UniquePublicKey $ Just pk)
                -- confEmail TODO
               
              let badField = conflUN <|> conflPK

              return (Error.openLeft $ User.AlreadyExists) -- conflict)

            Just userId ->
              User.setData userId App.Content.empty now >>= \case
                Left err ->
                  return (Error.openLeft err)

                Right () ->
                  App.createWithPlaceholder userId now <&> \case
                    Left err             -> Error.relaxedLeft err
                    Right (_, subdomain) -> Right (userId, subdomain)

  createWithPassword username password email now =
    Password.hashPassword password >>= \case
      Left err ->
        return (Error.openLeft err)

      Right secretDigest ->
        User
          { userPublicKey     = Nothing
          , userAlgorithm     = Nothing
          , userUsername      = username
          , userEmail         = Just email
          , userRole          = Regular
          , userActive        = True
          , userHerokuAddOnId = Nothing
          , userSecretDigest  = Just secretDigest
          , userDataRoot      = App.Content.empty
          , userInsertedAt    = now
          , userModifiedAt    = now
          }
          |> insertUnique
          |> bind \case
            Nothing ->
              return (Error.openLeft User.AlreadyExists)

            Just userId ->
              now
                |> App.createWithPlaceholder userId
                |> fmap \case
                  Left err -> Error.relaxedLeft err
                  Right _  -> Right userId

  createWithHeroku herokuUUID herokuRegion username password now =
    Heroku.AddOn.create herokuUUID herokuRegion now >>= \case
      Left err ->
        return . Left <| openUnionLift err

      Right herokuAddOnId ->
        Password.hashPassword password >>= \case
          Left err ->
            return . Left <| openUnionLift err

          Right secretDigest ->
            User
              { userPublicKey     = Nothing
              , userAlgorithm     = Nothing
              , userUsername      = username
              , userEmail         = Nothing
              , userRole          = Regular
              , userActive        = True
              , userHerokuAddOnId = Just herokuAddOnId
              , userSecretDigest  = Just secretDigest
              , userDataRoot      = App.Content.empty
              , userInsertedAt    = now
              , userModifiedAt    = now
              }
            |> insertUnique
            |> bind \case
              Just userID ->
                return (Right userID)

              Nothing ->
                return . Left <| openUnionLift User.AlreadyExists
