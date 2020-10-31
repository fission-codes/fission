module Fission.IPFS.PubSub.Topic.Types (Topic (..)) where

import           Servant.API

import           Fission.Prelude

newtype Topic = Topic { unTopic :: Text }
  deriving newtype ( Eq
                   , Ord
                   , Show
                   , Display
                   , ToHttpApiData
                   , FromHttpApiData
                   )
