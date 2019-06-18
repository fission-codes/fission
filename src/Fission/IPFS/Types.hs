module Fission.IPFS.Types
  ( Opt
  , Address
  , mkAddress
  , Peer (..)
  , Path (..)
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Aeson
import Data.Aeson.TH
import Data.Swagger (ToSchema)

import qualified Data.ByteString.Builder as Builder
import           Servant
import           System.Envy

import qualified Fission.Internal.UTF8 as UTF8

type Opt = String

newtype Address = Address { unaddress :: Lazy.ByteString }
  deriving          (Show, Generic)
  deriving anyclass ToSchema
  deriving newtype  IsString

newtype Path = Path { getPath :: FilePath }
  deriving         (Show, Generic)
  deriving anyclass ToSchema
  deriving newtype  IsString

newtype Peer = Peer { peer :: Text }
  deriving         (Show, Generic)
  deriving anyclass ToSchema
  deriving newtype  IsString

$(deriveJSON defaultOptions ''Peer)

instance FromEnv Path where
  fromEnv = Path <$> env "IPFS_PATH"

instance Display Address where
  display = Utf8Builder . Builder.lazyByteString . unaddress

instance MimeRender PlainText Address where
  mimeRender _proxy = unaddress

instance MimeRender OctetStream Address where
  mimeRender _proxy = unaddress

-- | Smart constructor for @Address@
mkAddress :: Lazy.ByteString -> Address
mkAddress = Address . UTF8.stripNewline
