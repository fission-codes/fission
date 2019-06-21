module Fission.IPFS.Types
  ( Opt
  , Address
  , mkAddress
  , Peer (..)
  , Path (..)
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Control.Lens ((.~), (?~))
import Data.Aeson
import Data.Aeson.TH
import Data.Swagger

import qualified Data.ByteString.Builder as Builder
import           Servant
import           System.Envy

import qualified Fission.Internal.UTF8 as UTF8

type Opt = String

newtype Address = Address { unaddress :: Lazy.ByteString }
  deriving         Show
  deriving newtype IsString

newtype Path = Path { getPath :: FilePath }
  deriving          ( Show
                    , Generic
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

newtype Peer = Peer { peer :: Text }
  deriving          ( Show
                    , Generic
                    )
  deriving newtype  ( IsString )

$(deriveJSON defaultOptions ''Peer)

instance FromEnv Path where
  fromEnv = Path <$> env "IPFS_PATH"

instance Display Address where
  display = Utf8Builder . Builder.lazyByteString . unaddress

instance MimeRender PlainText Address where
  mimeRender _proxy = unaddress

instance MimeRender OctetStream Address where
  mimeRender _proxy = unaddress

instance ToSchema Address where
  declareNamedSchema _ =
     return $ NamedSchema (Just "IPFS Address") $ mempty
            & type_   .~ SwaggerString
            & example ?~ "QmW2WQi7j6c7UgJTarActp7tDNikE4B2qXtFCfLPdsgaTQ"

instance ToSchema Peer where
  declareNamedSchema _ =
     return $ NamedSchema (Just "IPFS Peer") $ mempty
            & type_   .~ SwaggerString
            & example ?~ "/ip4/178.62.158.247/tcp/4001/ipfs/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd"

-- | Smart constructor for @Address@
mkAddress :: Lazy.ByteString -> Address
mkAddress = Address . UTF8.stripNewline
