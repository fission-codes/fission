module Fission.App.Name
  ( toSubdomain
  , module Fission.App.Name.Types
  , module Fission.App.Name.Error
  ) where

import           Fission.Prelude

import qualified Fission.URL.Subdomain.Types as URL

import           Fission.App.Name.Error
import           Fission.App.Name.Types

toSubdomain :: Name -> URL.Subdomain
toSubdomain name = URL.Subdomain $ raw name
