module Fission.Web.Server.Relay.Channel
  ( toReadChan
  , module Fission.Web.Server.Relay.Channel.Types
  ) where

import           Fission.Prelude

import           Fission.Web.Server.Relay.Channel.Types

toReadChan :: ChannelIn -> STM ChannelOut
toReadChan (ChannelIn rawChanIn) = do
  rawChanOut <- dupTChan rawChanIn
  return $ ChannelOut rawChanOut
