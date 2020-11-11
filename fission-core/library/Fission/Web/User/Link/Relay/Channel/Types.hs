module Fission.Web.User.Link.Relay.Channel.Types
  ( ChannelIn  (..)
  , ChannelOut (..)
  ) where

import           Control.Concurrent.STM.TChan
import qualified Network.WebSockets           as WS

newtype ChannelIn  = ChannelIn  { getChanIn :: TChan WS.DataMessage }
newtype ChannelOut = ChannelOut { getChanOut :: TChan WS.DataMessage }
