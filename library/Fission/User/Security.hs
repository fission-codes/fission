module Fission.User.Security
  ( hashID
  , genID
  ) where

import qualified RIO.Text       as Text

import           Fission.Prelude
import qualified Fission.Random  as Random
import           Fission.Security

-- | Generate an ID
genID :: MonadIO m => m SecretDigest
genID =
  50
    |> Random.alphaNum
    |> fmap hashID
    |> liftIO

-- | Create a 'SecretDigest' from some data, such as a users ID
--   Barely an obsfucating technique, but enough to hide DB ordering
hashID :: Digestable a => a -> SecretDigest
hashID digestable =
  digestable
    |> digest
    |> Text.take 20
