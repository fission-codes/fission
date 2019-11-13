module Fission.Environment.Storage.Types (Environment (..)) where

import Database.Selda.PostgreSQL

import Fission.Prelude
import Fission.Internal.Orphanage.PGConnectInfo ()

-- | Configuration for the web application
data Environment = Environment
  { pgConnectInfo  :: !PGConnectInfo  -- ^ PostgreSQL configuration
  , stripeCount    :: !Int             -- ^ Number of database stripes
  , connsPerStripe :: !Int             -- ^ Maximum number of concurrent connections per stripe
  , connTTL        :: !NominalDiffTime -- ^ Maxiumum connection time
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "Storage.Environment" \obj -> do
    pgConnectInfo  <- obj .: "postgresql" >>= parseJSON . Object
    stripeCount    <- obj .: "stripe_count"
    connsPerStripe <- obj .: "conns_per_stripe"
    connTTL        <- obj .: "conn_ttl"

    return <| Environment {..}
