module Fission.User.Query
  ( bySecret
  , byUsername
  ) where

import Database.Selda

import Fission.Prelude
import Fission.Storage.Query
import Fission.User.Types

-- | Find a user by their account secret
bySecret :: Text -> Row s User -> Col s Bool
bySecret secret user = user `is'` #active
                   .&& user ! #secretDigest .== text secret

-- | Find a user by their account secret
byUsername :: Text -> Row s User -> Col s Bool
byUsername username user = user `is'` #active
                       .&& user ! #username .== text username
