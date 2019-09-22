module Fission.Storage.Config.Types
  ( Config (..)
  , connTTL
  , connsPerStripe
  , pgConnectInfo
  , stripeCount
  ) where

import RIO
import RIO.Time

import Control.Lens (makeLenses)
import Data.Aeson
import Database.Selda.PostgreSQL

import Fission.Internal.Orphanage.PGConnectInfo ()

-- | Configuration for the web application
data Config = Config
  { _pgConnectInfo  :: !PGConnectInfo  -- ^ PostgreSQL configuration
  , _stripeCount    :: !Int             -- ^ Number of database stripes
  , _connsPerStripe :: !Int             -- ^ Maximum number of concurrent connections per stripe
  , _connTTL        :: !NominalDiffTime -- ^ Maxiumum connection time
  } deriving Show

makeLenses ''Config

instance FromJSON Config where
  parseJSON = withObject "Storage.Config" \obj -> do
    _pgConnectInfo  <- obj .: "postgresql" >>= parseJSON . Object
    _stripeCount    <- obj .: "stripeCount"
    _connsPerStripe <- obj .: "connsPerStripe"
    _connTTL        <- obj .: "connTTL"

    return $ Config {..}
