module Fission.CLI.PubSub.Secure.Payload.Class (MonadSecured (..)) where

import           Fission.Prelude

import qualified Fission.CLI.PubSub.Secure.Payload.Error  as SecurePayload
import           Fission.CLI.PubSub.Secure.Payload.Family

class
  ( ToJSON   (SecurePayload cipher msg)
  , FromJSON (SecurePayload cipher msg)
  )
  => MonadSecured m cipher msg where
  toSecurePayload   :: cipher -> msg -> m (SecurePayload cipher msg)
  fromSecurePayload :: cipher -> SecurePayload cipher msg -> m (Either SecurePayload.Error msg)
