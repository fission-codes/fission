module Fission.PubSub
  ( listen
  , broadcast
  , module Fission.PubSub.Class
  , module Fission.PubSub.Topic.Types
  ) where

import           Fission.Prelude

import           Fission.PubSub.Class
import           Fission.PubSub.Topic.Types

listen ::
  ( MonadPubSub m
  , MonadRaise  m
  , m `Raises` String
  , FromJSON msg
  )
  => Connection m
  -> m msg
listen conn = do
  bs <- receiveLBS conn
  ensure $ eitherDecode bs

broadcast ::
  ( MonadPubSub m
  , ToJSON msg
  )
  => Connection m
  -> msg
  -> m ()
broadcast conn msg = sendLBS conn $ encode msg
