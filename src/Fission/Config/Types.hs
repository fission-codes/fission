{-# LANGUAGE UndecidableInstances #-}
-- | Configuration types
module Fission.Config.Types
  ( Config (..)
  , Logger (..)
  , processCtx
  , logFunc
  , ipfsPath
  , host
  , dbPath
  , dbPool
  , herokuID
  , herokuPassword
  ) where

import RIO
import RIO.List (intercalate)
import RIO.Process (ProcessContext)

import Control.Lens (makeLenses)
import Data.Has

import           Fission.Web.Types
import           Fission.Log
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Storage.Types         as DB
import qualified Fission.Platform.Heroku.Types as Heroku

-- | The top level 'Fission' application 'RIO' configuration
data Config = Config
  { _processCtx     :: !ProcessContext
  , _logger         :: !Logger
  , _ipfsPath       :: !IPFS.BinPath
  , _host           :: !Host
  , _dbPath         :: !DB.Path
  , _dbPool         :: !DB.Pool
  , _herokuID       :: !Heroku.ID
  , _herokuPassword :: !Heroku.Password
  }

makeLenses ''Config

instance Show Config where
  show Config {..} = intercalate "\n"
    [ "Config {"
    , "  _processCtx     = **SOME PROC CONTEXT**"
    , "  _logFunc        = **SOME LOG FUNCTION**"
    , "  _ipfsPath       = " <> show _ipfsPath
    , "  _host           = " <> show _host
    , "  _dbPath         = " <> show _dbPath
    , "  _dbPool         = " <> show _dbPool
    , "  _herokuID       = " <> show _herokuID
    , "  _herokuPassword = " <> show _herokuPassword
    , "}"
    ]

instance Has ProcessContext Config where
  hasLens = processCtx

instance Has Logger Config where
  hasLens = logger

instance HasLogFunc Config where
  logFuncL = hasLens . logFunc

instance Has IPFS.BinPath Config where
  hasLens = ipfsPath

instance Has DB.Path Config where
  hasLens = dbPath

instance Has DB.Pool Config where
  hasLens = dbPool

instance Has Heroku.ID Config where
  hasLens = herokuID

instance Has Heroku.Password Config where
  hasLens = herokuPassword

instance Has Host Config where
  hasLens = host
