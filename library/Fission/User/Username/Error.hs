-- | Username errors

module Fission.User.Username.Error (Invalid (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error
import qualified Fission.Internal.UTF8 as UTF8

data Invalid = Invalid
  deriving ( Show
           , Eq
           , Exception
           )

instance Display Invalid where
  display Invalid = "Invalid username (must be alphanumeric separated with hyphens)"

instance ToServerError Invalid where
  toServerError Invalid =
    err422 { errBody = UTF8.showLazyBS <| textDisplay Invalid }
