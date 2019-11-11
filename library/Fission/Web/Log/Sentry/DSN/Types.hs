module Fission.Web.Log.Sentry.DSN.Types (DSN (..)) where

import           RIO
import qualified RIO.Text as Text

import Data.Aeson

newtype DSN = DSN { getDSN :: String }

instance Show DSN where
  show _ = "<Sentry DSN (key hidden)>"

instance FromJSON DSN where
  parseJSON = withText "Sentry.DSN" \txt -> return $ DSN $ Text.unpack txt
