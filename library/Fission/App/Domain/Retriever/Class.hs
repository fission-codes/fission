module Fission.App.Domain.Retriever.Class
  ( Retriever (..)
  , Errors
  , isOwnedBy
  , guardOwnedBy
  ) where

import           Database.Esqueleto

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import           Fission.Models.Error
import           Fission.Error

type Errors = OpenUnion
  '[ NotFound            App
   ]

class Monad m => Retriever m where
  allForApp   :: AppId -> m [Entity AppDomain]
  allByDomain :: DomainName -> Maybe Subdomain -> m [Entity AppDomain]

instance MonadIO m => Retriever m where
  allForApp appId =
    getEntity appId <&> \case
      Nothing -> openLeft <| NotFound @App
      Just appDomains -> Right app -- FIXME okay I'm clearly tired. Make tyhis work wit the type

isOwnedBy :: UserId -> App -> Bool
isOwnedBy userId App {ownerId} = userId == appOwnerId

guardOwnedBy ::
     UserId
  -> Entity App
  -> (Entity App -> m (Either (ActionNotAuthorized App) a))
  -> m (Either (ActionNotAuthorized App) a)
guardOwnedBy userId appEntity@(Entity _ app) cont =
  if isOwnedBy userId app
    then cont appEntity
    else Left <| ActionNotAuthorized @App userId
