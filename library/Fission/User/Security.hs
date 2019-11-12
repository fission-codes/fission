module Fission.User.Security
  ( hashID
  , genID
  ) where

import           Database.Selda (ID)
import qualified RIO.Text       as Text

import           Fission.Prelude
import qualified Fission.Random     as Random
import           Fission.Security
import           Fission.User.Types

-- | Create a 'SecretDigest' from the users ID
--   Barely an obsfucating technique, but enough to hide DB ordering
hashID :: ID User -> SecretDigest
hashID = Text.take 20 . digest

genID :: IO SecretDigest
genID = Text.take 20 . digest <$> Random.text 200
