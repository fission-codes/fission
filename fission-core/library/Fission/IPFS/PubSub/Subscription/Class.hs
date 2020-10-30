module Fission.IPFS.PubSub.Subscription.Class (SubscribesTo (..)) where

import           Fission.Prelude

import           Fission.IPFS.PubSub.Subscription.Message.Types
import           Fission.IPFS.PubSub.Topic.Types

class SubscribesTo m a where
 subscribeWithQueue :: Topic -> TQueue (Either String (Message a)) -> m (Async ())
 --FIXME better error type              ^^^^^^^^^^^^^^^
