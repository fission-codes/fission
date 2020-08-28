-- | Application security
module Fission.Security
  ( mkSecret
  , toSecret
  , Digestable (..)
  , module Fission.Security.Types
  ) where

import           Crypto.Hash
import qualified Data.ByteString.Random as BS

import qualified Fission.Internal.UTF8  as UTF8
import           Fission.Prelude
import           Fission.Security.Types

mkSecret :: Natural -> IO (Either UnicodeException Secret)
mkSecret = pure . toSecret <=< BS.random

toSecret :: ByteString -> Either UnicodeException Secret
toSecret raw = Secret <$> UTF8.encode raw

-- | Create a digest
class Digestable a where
  digest :: a -> SecretDigest

instance Digestable ByteString where
  digest bs = UTF8.textShow (hash bs :: Digest SHA3_512)

instance Digestable Text where
  digest = digest . encodeUtf8
