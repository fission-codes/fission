module Fission.Web.User.Link.Relay.Store.Types (Store (..)) where

import           Fission.Prelude

import           Fission.User.DID.Types
import           Fission.Web.User.Link.Relay.Channel.Types

newtype Store = Store { getRelays :: HashMap DID ChannelIn }

instance Semigroup Store where
  Store hmA <> Store hmB = Store (hmA <> hmB)

instance Monoid Store where
  mempty = Store mempty
