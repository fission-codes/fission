module Fission.User.Creator.Class
  ( Creator (..)
  , Errors
  ) where

import           Data.UUID (UUID)
import           Database.Esqueleto

import           Network.IPFS.CID.Types

import           Fission.Models
import           Fission.Prelude

import qualified Fission.Error as Error

import qualified Fission.Platform.Heroku.Region.Types  as Heroku
import qualified Fission.Platform.Heroku.AddOn.Creator as Heroku.AddOn

import qualified Fission.User.Creator.Error as User
import qualified Fission.User.Password      as Password
import           Fission.User.DID           as DID

import           Fission.User.Username.Types
import           Fission.User.Role.Types
import           Fission.User.Email.Types

import qualified Fission.App.Creator as App
import           Fission.URL.Subdomain.Types





import Fission.App.Content.Class as App.DefaultContent
import Fission.App.Domain.Class


import           Fission.IPFS.DNSLink




type Errors = OpenUnion
  '[ User.AlreadyExists
   , Heroku.AddOn.Error
   , Password.FailedDigest
   ]

class Heroku.AddOn.Creator m => Creator m where
  -- | Create a new, timestamped entry
  create ::
       Username
    -> DID
    -> Email
    -> UTCTime
    -> m (Either Errors (UserId, Subdomain))

  -- | Create a new, timestamped entry and heroku add-on
  createWithHeroku ::
       UUID
    -> Heroku.Region
    -> Username
    -> Text
    -> UTCTime
    -> m (Either Errors UserId)

instance
  ( MonadIO           m
  , MonadDNSLink      m
  , HasBaseAppDomain  m
  , HasBaseAppContent m
  )
  => Creator (Transaction m) where
  create username did email now =
    User
      { userDid           = Just did
      , userUsername      = username
      , userEmail         = Just email
      , userRole          = Regular
      , userActive        = True
      , userHerokuAddOnId = Nothing
      , userSecretDigest  = Nothing
      , userDataRoot      = blankCID
      , userInsertedAt    = now
      , userModifiedAt    = now
      }
      |> insertUnique
      |> bind \case
        Nothing ->
          return (Error.openLeft User.AlreadyExists)

        Just userId ->
          App.createWithPlaceholder userId now >>= \case
            Left err             -> undefined
            Right (_, subdomain) -> return <| Right (userId, subdomain)

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
              { userDid           = Nothing
              , userUsername      = username
              , userEmail         = Nothing
              , userRole          = Regular
              , userActive        = True
              , userHerokuAddOnId = Just herokuAddOnId
              , userSecretDigest  = Just secretDigest
              , userDataRoot      = blankCID
              , userInsertedAt    = now
              , userModifiedAt    = now
              }
            |> insertUnique
            |> bind \case
              Just userID ->
                return (Right userID)

              Nothing ->
                return . Left <| openUnionLift User.AlreadyExists

blankCID :: CID
blankCID = CID "Qmc5m94Gu7z62RC8waSKkZUrCCBJPyHbkpmGzEePxy2oXJ" -- FIXME! Move & change to base FFS
