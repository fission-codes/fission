module Fission.IPFS.Types
  ( BinPath (..)
  , CID (..)
  , mkCID
  , Name (..)
  , Opt
  , Peer (..)
  , Path (..)
  , SparseTree (..)
  , Tag (..)
  ) where

import           RIO
import qualified RIO.Text as Text

import Control.Lens ((.~), (?~))
import Data.Aeson
import Data.Aeson.TH
import Data.Swagger ( NamedSchema (..)
                    , SwaggerType (..)
                    , ToParamSchema
                    , ToSchema (..)
                    , type_
                    , example
                    )

import Servant
import System.Envy

import qualified Fission.Internal.UTF8 as UTF8

type Opt  = String

-- | CID path
--
-- Exmaple
--
-- > "QmcaHAFzUPRCRaUK12dC6YyhcqEEtdfg94XrPwgCxZ1ihD/myfile.txt"
newtype Path = Path { unpath :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    , Ord
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance MimeRender PlainText Path where
  mimeRender _ = UTF8.textToLazyBS . unpath

instance MimeRender OctetStream Path where
  mimeRender _ = UTF8.textToLazyBS . unpath

newtype Name = Name { unName :: String }
  deriving          ( Eq
                    , Generic
                    , Show
                    , Ord
                    )
  deriving anyclass ( ToParamSchema
                    , ToSchema
                    )
  deriving newtype  ( IsString )

$(deriveJSON defaultOptions ''Name)

instance FromHttpApiData Name where
  parseUrlPiece = \case
    ""  -> Left "Empty Name field"
    txt -> Right . Name $ Text.unpack txt

newtype CID = CID { unaddress :: Text }
  deriving          ( Eq
                    , Ord
                    , Show
                    )
  deriving newtype  ( IsString )

$(deriveJSON defaultOptions ''CID)

-- | Smart constructor for @CID@
mkCID :: Text -> CID
mkCID = CID . Text.strip

instance ToSchema CID where
  declareNamedSchema _ =
     return $ NamedSchema (Just "IPFS Address") $ mempty
            & type_   .~ SwaggerString
            & example ?~ "QmW2WQi7j6c7UgJTarActp7tDNikE4B2qXtFCfLPdsgaTQ"

data Tag
  = Key Name
  | Hash CID
  deriving ( Eq
           , Generic
           , Ord
           , Show
           )

$(deriveJSON defaultOptions ''Tag)

instance FromJSONKey Tag
instance ToJSONKey Tag
instance ToSchema Tag

instance FromHttpApiData Tag where
  parseUrlPiece txt = Key <$> parseUrlPiece txt

-- | Path to the IPFS binary
newtype BinPath = BinPath { getBinPath :: FilePath }
  deriving          ( Show
                    , Generic
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromEnv BinPath where
  fromEnv = BinPath <$> env "IPFS_PATH"

newtype Peer = Peer { peer :: Text }
  deriving          ( Show
                    , Generic
                    )
  deriving newtype  ( IsString )

$(deriveJSON defaultOptions ''Peer)

instance ToSchema Peer where
  declareNamedSchema _ =
     return $ NamedSchema (Just "IPFS Peer") $ mempty
            & type_   .~ SwaggerString
            & example ?~ "/ip4/178.62.158.247/tcp/4001/ipfs/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd"

instance Display CID where
  textDisplay = unaddress

instance MimeRender PlainText CID where
  mimeRender _ = UTF8.textToLazyBS . unaddress

instance MimeRender OctetStream CID where
  mimeRender _ = UTF8.textToLazyBS . unaddress

-- | Directory structure for CIDs and other identifiers
--
-- Examples:
--
-- > Content "abcdef"
--
-- > show $ Directory [(Key "abcdef", Stub "myfile.txt")])]
-- "abcdef/myfile.txt"
data SparseTree
  = Stub Name
  | Content CID
  | Directory (Map Tag SparseTree)
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToSchema )

$(deriveJSON defaultOptions ''SparseTree)
