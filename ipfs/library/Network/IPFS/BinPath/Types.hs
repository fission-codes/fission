module Network.IPFS.BinPath.Types (BinPath (..)) where

import qualified RIO.Text as Text

import System.Envy

import Network.IPFS.Internal.Orphanage.Natural ()
import Network.IPFS.Prelude

-- | Path to the IPFS binary
newtype BinPath = BinPath { getBinPath :: FilePath }
  deriving          ( Show
                    , Eq
                    , Generic
                    )
  deriving newtype  ( IsString )

instance FromEnv BinPath where
  fromEnv _ = BinPath <$> env "IPFS_PATH"

instance FromJSON BinPath where
  parseJSON = withText "IPFS.BinPath" \txt ->
    BinPath <$> parseJSON (String txt)

instance Display BinPath where
  textDisplay (BinPath path) = Text.pack path
