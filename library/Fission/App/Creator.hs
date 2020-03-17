module Fission.App.Creator
  ( module Fission.App.Creator.Class
  , createWithPlaceholder
  ) where

import           Fission.Prelude
import           Fission.Models

import           Fission.App.Creator.Class
import           Fission.App.Content as AppCID

import           Fission.URL.Subdomain.Types

createWithPlaceholder ::
  ( AppCID.Initializer m
  , Creator            m
  )
  => UserId
  -> UTCTime
  -> m (Either Errors (AppId, Subdomain))
createWithPlaceholder ownerId now = do
  defaultCID <- AppCID.placeholder
  create ownerId defaultCID now
