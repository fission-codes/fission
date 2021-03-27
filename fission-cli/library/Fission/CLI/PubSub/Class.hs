module Fission.CLI.PubSub.Class
  ( MonadPubSub (..)
  , module Fission.CLI.PubSub.Topic.Types
  ) where

import           Data.Kind
import qualified RIO.ByteString.Lazy            as Lazy

import           Servant.Client.Core            (BaseUrl (..))

import           Fission.Prelude

import           Fission.CLI.PubSub.Topic.Types

class Monad m => MonadPubSub m where
  type Connection m :: Type

  connect :: BaseUrl -> Topic -> (Connection m -> m a) -> m a

  sendLBS    :: Connection m ->   Lazy.ByteString -> m ()
  receiveLBS :: Connection m -> m Lazy.ByteString
