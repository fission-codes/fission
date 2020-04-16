module Fission.Environment.Storage.Types (Environment (..)) where

import Fission.Prelude
import Fission.Storage.PostgreSQL.ConnectionInfo.Types

-- | Configuration for the web application
data Environment = Environment
  { pgConnectInfo  :: !ConnectionInfo  -- ^ PostgreSQL configuration
  , stripeCount    :: !Int             -- ^ Number of database stripes
  , connsPerStripe :: !Int             -- ^ Maximum number of concurrent connections per stripe
  , connTTL        :: !NominalDiffTime -- ^ Maxiumum connection time
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "Storage.Environment" \obj -> do
    pgConnectInfo  <- obj .: "postgresql"
    stripeCount    <- obj .: "stripe_count"
    connsPerStripe <- obj .: "conns_per_stripe"
    connTTL        <- obj .: "conn_ttl"

    return Environment {..}
