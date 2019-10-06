module Fission.IPFS.Path.Types (Path (..)) where

import RIO

import Data.Swagger (ToSchema (..))
import Servant

import qualified Fission.Internal.UTF8 as UTF8

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
  deriving newtype  ( IsString
                    , ToHttpApiData
                    , ToSchema
                    )

instance MimeRender PlainText Path where
  mimeRender _ = UTF8.textToLazyBS . unpath

instance MimeRender OctetStream Path where
  mimeRender _ = UTF8.textToLazyBS . unpath
