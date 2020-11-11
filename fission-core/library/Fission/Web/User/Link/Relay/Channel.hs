module Fission.Web.User.Link.Relay.Channel
  ( toReadChan
  , module Fission.Web.User.Link.Relay.Channel.Types
  ) where

import           Fission.Prelude

import           Fission.Web.User.Link.Relay.Channel.Types

toReadChan :: ChannelIn -> STM ChannelOut
toReadChan (ChannelIn rawChanIn) = do
  rawChanOut <- dupTChan rawChanIn
  return $ ChannelOut rawChanOut
