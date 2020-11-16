-- |

module Fission.PubSub.DM.Channel.Types (Channel (..)) where

import qualified Crypto.PubKey.RSA          as RSA

import           Fission.Prelude

import           Fission.Key.GenData.Family

data Channel = Channel
  { partnerPK :: RSA.PublicKey
  , yourSK    :: RSA.PrivateKey
  } deriving Eq

type instance GenData Channel = RSA.PublicKey
