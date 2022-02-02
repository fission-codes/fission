module Fission.Web.Server.PowerDNS.Types
  ( URL (..)
  , ApiKey (..)
  ) where

import           Fission.Prelude

newtype URL = URL { getUrl :: Text }
  deriving newtype ( Eq
                   , Show
                   , FromJSON
                   , IsString
                   )

newtype ApiKey = ApiKey { getApiKey :: Text }
  deriving newtype ( Eq
                   , Show
                   , FromJSON
                   , IsString
                   )
