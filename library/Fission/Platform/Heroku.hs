module Fission.Platform.Heroku
  ( host
  , module Fission.Platform.Heroku.Authorizer
  ) where

import Fission.Prelude
import Fission.Platform.Heroku.Authorizer

host :: String
host = "api.heroku.com"
