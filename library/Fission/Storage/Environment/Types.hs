module Fission.Storage.Environment.Types
  ( Environment (..)
  -- , connTTL
  -- , connsPerStripe
  -- , pgConnectInfo
  -- , stripeCount
  ) where

import RIO
import RIO.Time

-- import Control.Lens (makeLenses)
import Data.Aeson
import Database.Selda.PostgreSQL

import Fission.Internal.Orphanage.PGConnectInfo ()

-- | Configuration for the web application
data Environment = Environment
  { _pgConnectInfo  :: !PGConnectInfo  -- ^ PostgreSQL configuration
  , _stripeCount    :: !Int             -- ^ Number of database stripes
  , _connsPerStripe :: !Int             -- ^ Maximum number of concurrent connections per stripe
  , _connTTL        :: !NominalDiffTime -- ^ Maxiumum connection time
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "Storage.Environment" \obj -> do
    _pgConnectInfo  <- obj .: "postgresql" >>= parseJSON . Object
    _stripeCount    <- obj .: "stripeCount"
    _connsPerStripe <- obj .: "connsPerStripe"
    _connTTL        <- obj .: "connTTL"

    return $ Environment {..}
