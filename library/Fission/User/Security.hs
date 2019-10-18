module Fission.User.Security
  ( hashID
  , genID
  ) where

import           RIO
import qualified RIO.Text as Text

import Database.Selda (ID)

import Fission.Security
import Fission.User.Types
import qualified Fission.Random as Random

-- | Create a 'SecretDigest' from the users ID
--   Barely an obsfucating technique, but enough to hide DB ordering
hashID :: ID User -> SecretDigest
hashID = Text.take 20 . digest

genID :: IO SecretDigest
genID = Text.take 20 . digest <$> Random.text 200
