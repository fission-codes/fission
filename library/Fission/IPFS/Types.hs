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
  , URL (..)
  ) where

import RIO

import Data.Swagger (ToSchema (..))
import Servant.Client as Client
import System.Envy

import Fission.Internal.Orphanage.Natural ()

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
  deriving anyclass ( ToSchema
                    , FromEnv
                    )
  deriving newtype  ( IsString )

newtype URL = URL { getURL :: Client.BaseUrl }
  deriving ( Eq
           , Generic
           , Show
           )

newtype Timeout = Timeout { getSeconds :: Natural }
  deriving          ( Eq
                    , Show
                    , Generic
                    )
  deriving anyclass FromEnv
