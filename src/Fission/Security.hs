{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Fission.Security
  ( Secret (..)
  , SecretDigest
  , mkSecret
  , toSecret
  , Digestable
  , digest
  ) where

import RIO

import           Crypto.Hash
import           Data.Aeson
import qualified Data.ByteString.Random as BS
import           Data.Swagger (ToSchema)

import qualified Fission.Internal.UTF8  as UTF8

type SecretDigest = Text

newtype Secret = Secret { unSecret :: Text }
  deriving          ( Eq
                    , Show
                    , Generic
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( FromJSON
                    , ToJSON
                    )

mkSecret :: Natural -> IO (Either UnicodeException Secret)
mkSecret = pure . toSecret <=< BS.random

toSecret :: ByteString -> Either UnicodeException Secret
toSecret raw = Secret <$> UTF8.encode raw

class Digestable a where
  digest :: a -> SecretDigest

instance Digestable ByteString where
  digest bs = UTF8.textShow (hash bs :: Digest SHA3_512)

instance Digestable Text where
  digest = digest . encodeUtf8
