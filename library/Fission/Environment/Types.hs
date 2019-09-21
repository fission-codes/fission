module Fission.Environment.Types (Environment (..)) where

import RIO hiding (timeout)

import Data.Aeson
import Database.Selda.PostgreSQL

import qualified Fission.IPFS.Config.Types as IPFS
import qualified Fission.Web.Config.Types  as Web

import Fission.Internal.Orphanage.PGConnectInfo ()

data Environment = Environment
  { web  :: !Web.Config
  , ipfs :: !IPFS.Config
  , pg   :: !PGConnectInfo
  }

instance FromJSON Environment where
  parseJSON = withObject "Environment" \obj -> do
    web  <- parseJSON $ Object obj
    ipfs <- parseJSON $ Object obj
    pg   <- parseJSON $ Object obj
    return $ Environment {..}
