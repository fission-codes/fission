module Fission.Storage.Environment.Types
  ( -- Environment (..)
  Fields
  -- , connTTL
  -- , connsPerStripe
  -- , pgConnectInfo
  -- , stripeCount
  ) where

import RIO
import RIO.Time

-- import Control.Lens              (makeLenses)
-- import Data.Aeson
import Database.Selda.PostgreSQL

import SuperRecord

import Fission.Internal.Orphanage.PGConnectInfo ()

-- | Configuration for the web application
type Fields =
  '[ "postgresql"     := PGConnectInfo   -- ^ PostgreSQL configuration
   , "stripeCount"    := Int             -- ^ Number of database stripes
   , "connsPerStripe" := Int             -- ^ Maximum number of concurrent connections per stripe
   , "connTTL"        := NominalDiffTime -- ^ Maxiumum connection time
   ]

-- makeLenses ''Environment

-- instance FromJSON Environment where
--   parseJSON = withObject "Storage.Environment" \obj -> do
--     _pgConnectInfo  <- obj .: "postgresql" >>= parseJSON . Object
--     _stripeCount    <- obj .: "stripeCount"
--     _connsPerStripe <- obj .: "connsPerStripe"
--     _connTTL        <- obj .: "connTTL"

--     return $ Environment {..}
