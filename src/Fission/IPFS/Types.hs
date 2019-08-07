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
  , Timeout (..)
  ) where

import RIO

import Data.Swagger (ToSchema (..))
import System.Envy

import Fission.IPFS.CID.Types
import Fission.IPFS.Name.Types
import Fission.IPFS.Path.Types
import Fission.IPFS.Peer.Types
import Fission.IPFS.Process.Types
import Fission.IPFS.SparseTree.Types

-- | Path to the IPFS binary
newtype BinPath = BinPath { getBinPath :: FilePath }
  deriving          ( Show
                    , Generic
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromEnv BinPath where
  fromEnv = BinPath <$> env "IPFS_PATH"

newtype Timeout = Timeout { getSeconds :: Natural }
  deriving Show

instance FromEnv Timeout where
  fromEnv = Timeout . fromIntegral <$> (env "IPFS_TIMEOUT" :: Parser Int)
