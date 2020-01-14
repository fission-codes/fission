module Fission.User.Creator.Error (AlreadyExists (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error
import qualified Fission.Internal.UTF8 as UTF8

data AlreadyExists = AlreadyExists
  deriving ( Show
           , Eq
           , Exception
           )

instance Display AlreadyExists where
  display AlreadyExists = "The username or email already exists in our system"

instance ToServerError AlreadyExists where
  toServerError AlreadyExists = err409 { errBody = UTF8.showLazyBS AlreadyExists }
