module Fission.Web.Server.Sentry.DSN.Types (DSN (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

newtype DSN = DSN { getDSN :: String }

instance Show DSN where
  show _ = "<Sentry DSN (key hidden)>"

instance FromJSON DSN where
  parseJSON = withText "Sentry.DSN" \txt -> return . DSN $ Text.unpack txt
