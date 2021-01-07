module Fission.CLI.PubSub.Secure.Payload.Family (SecurePayload) where

import           Data.Kind

type family SecurePayload cipher expected :: Type
