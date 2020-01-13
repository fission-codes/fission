-- | TODO make me pluckable
module Fission.User.Creator.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import qualified Fission.Web.Error     as Web -- TODO move to .Class and drop qualified
import qualified Fission.Internal.UTF8 as UTF8

data Error
  = FailedDigest
  | AlreadyExists
  deriving ( Show
           , Eq
           , Exception
           )

instance Display Error where
  display FailedDigest = "Could not create password digest"
  display AlreadyExists = "The username or email already exists in our system"

instance Web.ToServerError Error where
  toServerError FailedDigest  = err500 { errBody = UTF8.showLazyBS FailedDigest }
  toServerError AlreadyExists = err409 { errBody = UTF8.showLazyBS AlreadyExists }
