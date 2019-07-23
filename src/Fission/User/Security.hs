module Fission.User.Security (hashID) where

import           RIO
import qualified RIO.Text as Text

-- import Database.Selda (ID)

import Fission.Security
import Fission.User.Types as User

-- | Create a 'SecretDigest' from the users ID
--   Barely an obsfucating technique, but enough to hide DB ordering
hashID :: User.ID -> SecretDigest
hashID = Text.take 20 . digest
