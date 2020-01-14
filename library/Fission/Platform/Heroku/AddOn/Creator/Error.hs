module Fission.Platform.Heroku.AddOn.Creator.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error
import qualified Fission.Internal.UTF8 as UTF8

data Error = AlreadyExists
  deriving ( Show
           , Eq
           , Exception
           )

instance Display Error where
  display AlreadyExists = "This Heroku AddOn has already been created"

instance ToServerError Error where
  toServerError AlreadyExists = err409 { errBody = UTF8.showLazyBS AlreadyExists }
