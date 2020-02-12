module Fission.Platform.Heroku.ID.Types (ID (..)) where

import Fission.Prelude

-- | Heroku add-on ID (from @addon-manifest.json@)
newtype ID = ID { getID :: ByteString }
  deriving         (Eq, Show)
  deriving newtype IsString
