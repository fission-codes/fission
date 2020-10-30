module Fission.IPFS.PubSub.Subscription.Client.Types (MessageStream (..)) where

import           Servant.API
import qualified Servant.Client.Streaming                       as Streaming
import           Servant.Server.Experimental.Auth
import           Servant.Types.SourceT

import           Fission.Prelude

import           Fission.IPFS.PubSub.Subscription.Message.Types

type MessageStream m a
  =  "api"
  :> "v0"
  :> "pubsub"
  :> "sub"
  :> StreamGet NewlineFraming JSON (SourceT m (Message a))
