module Fission.Web.Server.Relay.Store.Types (Store (..)) where

import           Fission.Prelude

import           Fission.Web.Server.Relay.Channel.Types
import           Web.DID.Types

newtype Store = Store { getRelays :: HashMap DID ChannelIn }

instance Semigroup Store where
  Store hmA <> Store hmB = Store (hmA <> hmB)

instance Monoid Store where
  mempty = Store mempty
