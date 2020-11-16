module Fission.PubSub.Secure.SecureConnection.Types (SecureConnection (..)) where

import           Fission.Prelude

import           Fission.PubSub.Class
import           Fission.PubSub.Secure.Class

data SecureConnection m cipher = SecureConnection
  { conn       :: Connection m
  , sessionKey :: cipher
  }
