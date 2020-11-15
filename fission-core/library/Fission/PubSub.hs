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
  => m msg
listen = do
  bs <- receive
  ensure $ eitherDecode bs

broadcast ::
  ( MonadPubSub m
  , MonadRaise        m
  , m `Raises` String
  , ToJSON msg
  )
  => msg
  -> m ()
broadcast msg = send $ toJSON msg
