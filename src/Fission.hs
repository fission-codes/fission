module Fission
  ( Fission
  , fromConfig
  , simply
  ) where

import RIO

import Data.Has

import           Fission.Types
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Log                   as Log
import qualified Fission.Platform.Heroku.Types as Heroku
import qualified Fission.Storage.Types         as DB
import qualified Fission.Web.Types             as Web

fromConfig :: (MonadReader cfg m, Has a cfg) => m a
fromConfig = view hasLens

simply :: RIO LogFunc a -> IO a
simply = runRIO (mkLogFunc Log.simple)
