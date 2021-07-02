{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.Server.Internal.Orphanage.ServerError () where

import           RIO
import qualified RIO.Text       as Text

import           Servant.Server

instance Display ServerError where
  display = displayShow

instance Display [ServerError] where
  textDisplay errs = Text.intercalate ", " $ fmap textDisplay errs
