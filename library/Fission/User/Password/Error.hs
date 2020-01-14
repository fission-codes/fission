module Fission.User.Password.Error (FailedDigest (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error
import qualified Fission.Internal.UTF8 as UTF8

data FailedDigest = FailedDigest
  deriving ( Show
           , Eq
           , Exception
           )

instance Display FailedDigest where
  display FailedDigest  = "Could not create password digest"

instance ToServerError FailedDigest where
  toServerError FailedDigest  = err500 { errBody = UTF8.showLazyBS FailedDigest }
