module Fission.Platform.Heroku.Auth.Types (Auth (..)) where

import Fission.Prelude

-- | An authorized Heroku Platform service
newtype Auth = Auth { unAuth :: ByteString }
  deriving (Eq, Show)
