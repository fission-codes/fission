module Fission.Web.Server.Relay.Channel.Types
  ( ChannelIn  (..)
  , ChannelOut (..)
  ) where

import           Control.Concurrent.STM.TChan
import qualified Network.WebSockets           as WS

import           Fission.Error.NotFound.Types

import           Fission.Prelude

newtype ChannelIn  = ChannelIn  { getChanIn  :: TChan WS.DataMessage }
newtype ChannelOut = ChannelOut { getChanOut :: TChan WS.DataMessage }

instance Display (NotFound ChannelIn) where
  display _ = "Unable to find matching link relay channel"
