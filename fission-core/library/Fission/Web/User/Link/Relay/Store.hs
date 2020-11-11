module Fission.Web.User.Link.Relay.Store
  ( getOrCreate
  -- * Reexports
  , module Fission.Web.User.Link.Relay.Store.Class
  , module Fission.Web.User.Link.Relay.Store.Types
  ) where

import           Control.Concurrent.STM.TChan

import qualified RIO.HashMap                               as HashMap

import           Fission.Prelude

import           Fission.User.DID.Types
import           Fission.Web.User.Link.Relay.Channel.Types

import           Fission.Web.User.Link.Relay.Store.Class
import           Fission.Web.User.Link.Relay.Store.Types

getOrCreate :: DID -> Store -> STM (ChannelIn, Store)
getOrCreate did store@(Store hashMap) =
  case HashMap.lookup did hashMap of
    Just chanIn -> do
      return (chanIn, store)

    Nothing -> do
      chanIn <- ChannelIn <$> newBroadcastTChan
      return (chanIn, Store $ HashMap.insert did chanIn hashMap)
