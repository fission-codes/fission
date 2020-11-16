module Fission.PubSub
  ( listen
  , broadcast
  , module Fission.PubSub.Class
  ) where

import           Fission.Prelude

import           Fission.PubSub.Class

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
  , MonadRaise  m
  , m `Raises` String
  , ToJSON msg
  )
  => Connection m
  -> msg
  -> m ()
broadcast conn msg = sendLBS conn $ encode msg
