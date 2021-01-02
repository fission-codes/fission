module Fission.Platform.Heroku.Password.Types (Password (..)) where

import Fission.Prelude

-- | Heroku add-on password (from @addon-manifest.json@)
newtype Password = Password { getPassword :: ByteString }
  deriving         (Eq, Show)
  deriving newtype IsString
