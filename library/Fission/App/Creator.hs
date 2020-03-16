module Fission.App.Creator
  ( module Fission.App.Creator.Class
  , createWithPlaceholder
  ) where

import           Fission.Prelude
import           Fission.Models

import           Fission.App.Creator.Class
import           Fission.App.Content.Class as App.DefaultContent
import           Fission.URL.Subdomain.Types

createWithPlaceholder ::
  ( HasBaseAppContent m
  , Creator m
  )
  => UserId
  -> UTCTime
  -> m (Either Errors (AppId, Subdomain))
createWithPlaceholder ownerId now = do
  defaultCID <- App.DefaultContent.get
  create ownerId defaultCID now
