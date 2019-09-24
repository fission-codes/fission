module Fission.IPFS.BinPath.Types where -- (BinPath (..)) where

-- import RIO

-- import Data.Aeson
-- import Data.Swagger (ToSchema (..))
-- import System.Envy

-- import Fission.Internal.Orphanage.Natural ()

-- -- | Path to the IPFS binary
-- newtype BinPath = BinPath { getBinPath :: FilePath }
--   deriving          ( Show
--                     , Generic
--                     )
--   deriving anyclass ( ToSchema )
--   deriving newtype  ( IsString
--                     , ToJSON
--                     , FromJSON
--                     )

-- instance FromEnv BinPath where
--   fromEnv _ = BinPath <$> env "IPFS_PATH"
