module Fission.PubSub.Topic.Types (Topic (..)) where

import           Fission.Prelude

newtype Topic = Topic { raw :: Text }
  deriving newtype (Show, Eq)
