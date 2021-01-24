module Fission.Web.Server.Relay.Store
  ( setup
  , getOrCreate
  -- * Reexports
  , module Fission.Web.Server.Relay.Store.Class
  , module Fission.Web.Server.Relay.Store.Types
  ) where

import           Control.Concurrent.STM.TChan

import qualified RIO.HashMap                          as HashMap
import           RIO.Set                              as Set

import           Fission.Prelude

import           Fission.User.DID.Types
import           Fission.Web.Server.Relay.Channel

import           Fission.Web.Server.Relay.Store.Class
import           Fission.Web.Server.Relay.Store.Types

setup :: DID -> TVar Store -> STM (TVar (Set a), ChannelIn, ChannelOut)
setup did storeVar = do
  store            <- readTVar storeVar
  (chanIn, store') <- getOrCreate did store
  chanOut          <- toReadChan chanIn
  sentBufferVar    <- newTVar Set.empty

  swapTVar storeVar store'
  return (sentBufferVar, chanIn, chanOut)

getOrCreate :: DID -> Store -> STM (ChannelIn, Store)
getOrCreate did store@(Store hashMap) =
  case HashMap.lookup did hashMap of
    Just chanIn ->
      return (chanIn, store)

    Nothing -> do
      chanIn <- ChannelIn <$> newBroadcastTChan
      return (chanIn, Store $ HashMap.insert did chanIn hashMap)
