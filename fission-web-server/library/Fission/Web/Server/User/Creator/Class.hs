module Fission.Web.Server.User.Creator.Class
  ( Creator (..)
  , Errors'
  ) where

import           Data.UUID                                          (UUID)
import           Servant

import qualified Network.IPFS.Add.Error                             as IPFS.Pin
import qualified Network.IPFS.Get.Error                             as IPFS.Stat

import           Fission.Prelude

import           Fission.Error                                      as Error
import           Fission.Key                                        as Key
import qualified Fission.Platform.Heroku.Region.Types               as Heroku
import           Fission.URL

import           Fission.User.Email.Types
import           Fission.User.Username                              as Username

import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import           Fission.Web.Server.Models

import qualified Fission.Web.Server.App.Domain                      as App.Domain
import qualified Fission.Web.Server.Heroku.AddOn.Creator            as Heroku.AddOn

import qualified Fission.Web.Server.User.Creator.Error              as User
import           Fission.Web.Server.User.Password                   as Password

type Errors' = OpenUnion
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

   , IPFS.Pin.Error
   , IPFS.Stat.Error

   , InvalidURL
   , ServerError
   ]

class Heroku.AddOn.Creator m => Creator m where
  -- | Create a new, timestamped entry
  create ::
       Username
    -> Key.Public
    -> Maybe Email
    -> UTCTime
    -> m (Either Errors' UserId)

  createWithPassword ::
       Username
    -> Password
    -> Maybe Email
    -> UTCTime
    -> m (Either Errors' UserId)

  -- | Create a new, timestamped entry and heroku add-on
  createWithHeroku ::
       UUID
    -> Heroku.Region
    -> Username
    -> Password
    -> UTCTime
    -> m (Either Errors' UserId)
