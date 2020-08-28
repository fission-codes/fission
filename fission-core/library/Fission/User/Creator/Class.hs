module Fission.User.Creator.Class
  ( Creator (..)
  , Errors'
  ) where

import           Data.UUID                             (UUID)
import           Servant

import           Fission.Error                         as Error
import           Fission.Models
import           Fission.Prelude

import           Fission.Key                           as Key
import           Fission.URL

import qualified Fission.Platform.Heroku.AddOn.Creator as Heroku.AddOn
import qualified Fission.Platform.Heroku.Region.Types  as Heroku

import qualified Fission.User.Creator.Error            as User
import           Fission.User.Password                 as Password
import           Fission.User.Types
import qualified Fission.User.Username                 as Username

import qualified Network.IPFS.Add.Error                as IPFS.Pin
import qualified Network.IPFS.Get.Error                as IPFS.Stat

import qualified Fission.App.Domain                    as App.Domain

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
    -> Email
    -> UTCTime
    -> m (Either Errors' UserId)

  createWithPassword ::
       Username
    -> Password
    -> Email
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
