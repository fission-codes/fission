module Fission.AWS.Types
  ( AccessKey (..)
  , SecretKey (..)
  ) where

import RIO

import Data.Aeson
import Data.Swagger (ToSchema (..))
import Servant

newtype AccessKey = AccessKey { getAccess :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    , Ord
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromJSON AccessKey where
  parseJSON = withText "AWS.AccessKey" \txt ->
    AccessKey <$> parseJSON (String txt)


newtype SecretKey = SecretKey { getSecret :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    , Ord
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromJSON SecretKey where
  parseJSON = withText "AWS.SecretKey" \txt ->
    SecretKey <$> parseJSON (String txt)
