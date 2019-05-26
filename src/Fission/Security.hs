{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Fission.Security
  ( Secret
  , SecretDigest
  , unSecret
  , mkSecret
  , toSecret
  ) where

import Data.Aeson
import RIO

import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.Random        as Random

type SecretDigest = Text

newtype Secret = Secret { unSecret :: Text }
  deriving ( Show
           , Eq
           , ToJSON
           , FromJSON
           )

mkSecret :: Int -> IO (Either UnicodeException Secret)
mkSecret = return . toSecret <=< Random.text

toSecret :: ByteString -> Either UnicodeException Secret
toSecret raw = Secret <$> UTF8.encode raw
