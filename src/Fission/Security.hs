{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Fission.Security
  ( Secret
  , SecretDigest
  , unSecret
  , mkSecret
  , toSecret
  , Digestable
  , digest
  ) where

import RIO

import Crypto.Hash
import Data.Aeson

import qualified Data.ByteString.Random as BS
import qualified Fission.Internal.UTF8  as UTF8

type SecretDigest = Text

newtype Secret = Secret { unSecret :: Text }
  deriving ( Show
           , Eq
           , ToJSON
           , FromJSON
           )

mkSecret :: Natural -> IO (Either UnicodeException Secret)
mkSecret = return . toSecret <=< BS.random

toSecret :: ByteString -> Either UnicodeException Secret
toSecret raw = Secret <$> UTF8.encode raw

class Digestable a where
  digest :: a -> SecretDigest

instance Digestable ByteString where
  digest bs = UTF8.textShow (hash bs :: Digest SHA3_512)

instance Digestable Text where
  digest = digest . encodeUtf8
