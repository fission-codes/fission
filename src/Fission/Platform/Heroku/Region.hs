{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Platform.Heroku.Region where

import RIO

import Data.Aeson
import Database.Selda

data Region
  = California
  | Dublin
  | Frankfurt
  | Oregon
  | Singapore
  | Sydney
  | Tokyo
  | Virginia
  -- | Other Text -- ^ Being very lenient for now
  deriving ( Show
           , Read
           , Eq
           , Enum
           , Bounded
           , SqlType
           )

instance ToJSON Region where
  toJSON = \case
    California -> String "amazon-web-services::us-west-1"
    Dublin     -> String "amazon-web-services::eu-west-1"
    Frankfurt  -> String "amazon-web-services::eu-central-1"
    Oregon     -> String "amazon-web-services::us-west-2"
    Singapore  -> String "amazon-web-services::ap-southeast-1"
    Sydney     -> String "amazon-web-services::ap-southeast-2"
    Tokyo      -> String "amazon-web-services::ap-northeast-1"
    Virginia   -> String "amazon-web-services::us-east-1"
    -- Other txt  -> String txt

instance FromJSON Region where
  parseJSON = withText "Region" $ \case
    "amazon-web-services::us-west-1"      -> return California
    "amazon-web-services::eu-west-1"      -> return Dublin
    "amazon-web-services::eu-central-1"   -> return Frankfurt
    "amazon-web-services::us-west-2"      -> return Oregon
    "amazon-web-services::ap-southeast-1" -> return Singapore
    "amazon-web-services::ap-southeast-2" -> return Sydney
    "amazon-web-services::ap-northeast-1" -> return Tokyo
    "amazon-web-services::us-east-1"      -> return Virginia
    -- other                                 -> return $ Other other
