-- | Web app config
module Fission.Web.Environment.Types
  ( -- Environment (..)
    Fields
  -- , host
  -- , isTLS
  -- , monitor
  -- , port
  -- , pretty
  ) where

import RIO

-- import Control.Lens (makeLenses)
-- import Data.Aeson

import Network.Wai.Handler.Warp
import SuperRecord

import qualified Fission.Web.Types as Web

-- | Configuration for the web application
type Fields = '[ "host"    := Web.Host -- ^ Web app's host
               , "port"    := Port      -- ^ Web app's port
               , "tls"     := Bool     -- ^ Run over TLS
               , "pretty"  := Bool     -- ^ Pretty-print requests
               , "monitor" := Bool     -- ^ Live monitor application
               ]

-- makeLenses ''Environment

-- instance FromJSON Environment where
--   parseJSON = withObject "Web Config" \obj -> do
--     _monitor <- obj .:? "monitor" .!= False
--     _pretty  <- obj .:? "pretty"  .!= False
--     _isTLS   <- obj .:? "tls"     .!= True
--     _port    <- obj .:? "port"    .!= if _isTLS then 443 else 80
--     _host    <- obj .:  "host"

--     return $ Environment {..}
