module Fission.Web.API.Heroku.Auth.Types (Auth (..)) where

import           Fission.Web.API.Prelude

-- | An authorized Heroku Platform service
newtype Auth = Auth { unAuth :: ByteString }
  deriving (Eq, Show)
