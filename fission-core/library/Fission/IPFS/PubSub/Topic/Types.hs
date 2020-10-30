module Fission.IPFS.PubSub.Topic.Types (Topic (..)) where

import           Fission.Prelude

newtype Topic = Topic { unTopic :: Text }
  deriving newtype (Show, Eq, Ord)
