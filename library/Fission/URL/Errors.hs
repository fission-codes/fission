module Fission.URL.Errors (InvalidURL (..)) where

import Servant.Server

import Fission.Prelude
import Fission.Web.Error.Class

data InvalidURL = InvalidURL
  deriving (Eq, Show)

instance Display InvalidURL where
  textDisplay _ = "Invalid URL"

instance ToServerError InvalidURL where
  toServerError err = err422 { errBody = displayLazyBS err }
