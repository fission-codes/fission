module Fission.IPFS.Types
  ( Opt
  , Address (..)
  , Peer (..)
  , Path (..)
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Aeson
import Data.Aeson.TH

import qualified Data.ByteString.Builder as Builder
import           Servant
import System.Envy

import qualified Fission.Internal.UTF8 as UTF8

type Opt = String

newtype Address = Address { unaddress :: Lazy.ByteString }

data Peer = Peer { peer :: Text }
$(deriveJSON defaultOptions ''Peer)

newtype Path = Path { getPath :: FilePath }
  deriving (Show, IsString)

instance FromEnv Path where
  fromEnv = Path <$> env "IPFS_PATH"

instance Display Address where
  display = Utf8Builder . Builder.lazyByteString . unaddress

instance MimeRender PlainText Address where
  mimeRender _proxy = unaddress

instance MimeRender OctetStream Address where
  mimeRender _proxy = unaddress
