module Fission.IPFS.BinPath.Types (BinPath (..)) where

import RIO

import Data.Aeson
import Data.Swagger (ToSchema (..))
import System.Envy

import Fission.Internal.Orphanage.Natural ()

-- | Path to the IPFS binary
newtype BinPath = BinPath { getBinPath :: FilePath }
  deriving          ( Show
                    , Generic
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromEnv BinPath where
  fromEnv _ = BinPath <$> env "IPFS_PATH"

instance FromJSON BinPath where
  parseJSON = withText "IPFS.BinPath" \txt ->
    BinPath <$> parseJSON (String txt)
