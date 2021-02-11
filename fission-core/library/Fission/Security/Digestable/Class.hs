module Fission.Security.Digestable.Class (Digestable (..)) where

import           Crypto.Hash

import           Fission.Prelude

import qualified Fission.Internal.UTF8  as UTF8

import           Fission.Security.Types

-- | Create a digest
class Digestable a where
  digest :: a -> SecretDigest

instance Digestable ByteString where
  digest bs = UTF8.textShow (hash bs :: Digest SHA3_512)

instance Digestable Text where
  digest = digest . encodeUtf8
