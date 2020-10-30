module Fission.IPFS.PubSub.Subscription
  ( subscribe
  , withQueue
  -- * Reexports
  , module Fission.IPFS.PubSub.Subscription.Class
  , module Fission.IPFS.PubSub.Subscription.Client
  , module Fission.IPFS.PubSub.Subscription.Message.Types
  ) where

import           Fission.Prelude

import           Fission.IPFS.PubSub.Subscription.Class
import           Fission.IPFS.PubSub.Subscription.Client
import           Fission.IPFS.PubSub.Subscription.Message.Types
import           Fission.IPFS.PubSub.Topic.Types

subscribe ::
  ( MonadIO m
  , m `SubscribesTo` a
  )
  => Topic
  -> m (TQueue (Either String (Message a)), Async ())
subscribe topic = do
  tq       <- liftIO $ atomically newTQueue
  listener <- subscribeWithQueue topic tq
  return (tq, listener)

withQueue ::
  ( MonadIO m
  , m `SubscribesTo` a
  )
  => Topic
  -> (TQueue (Either String (Message a)) -> m a)
  -> m a
withQueue topic reactor = do
  (tq, listener) <- subscribe topic
  result         <- reactor tq

  cancel listener
  return result
