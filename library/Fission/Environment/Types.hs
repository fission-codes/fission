-- | External app configuration ("knobs")
module Fission.Environment.Types
  ( Fields -- Environment (..)
  -- , ipfs
  -- , storage
  -- , web
  ) where

-- import RIO hiding (timeout)

-- import Control.Lens (makeLenses)
-- import Data.Aeson
import SuperRecord  as SR

import qualified Fission.IPFS.Config.Types              as IPFS
-- import qualified Fission.Platform.Heroku.AddOn.Manifest as Heroku
import qualified Fission.Storage.Environment.Types      as Storage
import qualified Fission.Web.Environment.Types          as Web

type Fields = '[ "ipfs"    := Rec IPFS.Fields    -- ^ IPFS configuration
               , "storage" := Rec Storage.Fields -- ^ Storage/DB configuration
               , "web"     := Rec Web.Fields     -- ^ Web configuration
               ]

-- -- makeLenses ''Environment

-- instance FromJSON Environment where
--   parseJSON = withObject "Environment" \obj -> do
--     _ipfs    <- parseJSON . Object =<< obj .: "ipfs"
--     _storage <- parseJSON . Object =<< obj .: "storage"
--     _web     <- parseJSON . Object =<< obj .: "web"

--     return $ Environment {..}
