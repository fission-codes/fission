module Fission.User.Query (bySecret) where

import RIO

import Database.Selda

import Fission.Internal.Orphanage ()
import Fission.Storage.Query

import Fission.User.Types

-- | Find a user by their account secret
--
--   TODO `limit 0 1`
-- bySecret :: MonadSelda m => Text -> m [User]

-- bySecret :: Relational s => Text -> Row s t -> Query s (Row s t)
bySecret user secret = do
      user `is'` #_active
  .&& user ! #_secretDigest .== text secret

  -- return user
