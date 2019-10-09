module Fission.IPFS.CID.Types
  ( CID (..)
  , mkCID
  ) where

import           RIO
import           RIO.Char
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

-- import Data.Text.Encoding as Text

import Control.Lens ((?~))
import Data.Aeson
import Data.Swagger ( NamedSchema   (..)
                    , SwaggerType   (..)
                    , ToParamSchema (..)
                    , ToSchema      (..)
                    , type_
                    , example
                    )
import Servant

import qualified Fission.Internal.UTF8 as UTF8

newtype CID = CID { unaddress :: Text }
  deriving          ( Eq
                    , Generic
                    , Ord
                    , Read
                    , Show
                    )
  deriving anyclass ( ToParamSchema )
  deriving newtype  ( IsString
                    , ToHttpApiData
                    )

instance ToJSON CID where
  toJSON (CID cid) = toJSON $ normalize cid
    where
      normalize (Text.take 1 -> "\"") = UTF8.stripN 1 cid
      normalize cid' = cid'

instance FromJSON CID where
  parseJSON = withText "ContentAddress" (pure . CID)

instance ToSchema CID where
  declareNamedSchema _ =
     return $ NamedSchema (Just "IPFS Address") $ mempty
            & type_   ?~ SwaggerString
            & example ?~ "QmW2WQi7j6c7UgJTarActp7tDNikE4B2qXtFCfLPdsgaTQ"

instance Display CID where
  textDisplay = unaddress

instance MimeRender PlainText CID where
  mimeRender _ = UTF8.textToLazyBS . unaddress

instance MimeRender OctetStream CID where
  mimeRender _ = UTF8.textToLazyBS . unaddress

instance MimeUnrender PlainText CID where
  mimeUnrender _proxy bs =
    case decodeUtf8' $ Lazy.toStrict bs of
      Left err  -> Left $ show err
      Right txt -> Right $ CID txt

instance MimeUnrender PlainText [CID] where
  mimeUnrender proxy bs = sequence cids
    where
      cids :: [Either String CID]
      cids = mimeUnrender proxy <$> Lazy.split (fromIntegral $ ord ',') bs

instance FromHttpApiData CID where
  parseUrlPiece = Right . CID

-- | Smart constructor for @CID@
mkCID :: Text -> CID
mkCID = CID . Text.strip
