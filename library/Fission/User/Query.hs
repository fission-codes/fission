module Fission.User.Query (bySecret) where

import RIO

import Database.Selda

import Fission.Storage.Query
import Fission.User.Types

-- | Find a user by their account secret
bySecret :: Text -> Row s User -> Col s Bool
bySecret secret user = (user `is'` #_active)
                   .&& (user ! #_secretDigest .== text secret)
