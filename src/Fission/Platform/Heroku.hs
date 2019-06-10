module Fission.Platform.Heroku
  ( host
  , AddOn (..)
  , addOns
  , Region (..)
  , UserConfig (..)
  ) where

import RIO

import Fission.Platform.Heroku.AddOn      (AddOn (..), addOns)
import Fission.Platform.Heroku.Region     (Region (..))
import Fission.Platform.Heroku.UserConfig (UserConfig (..))

host :: String
host = "api.heroku.com"
