-- |

module Fission.Key.GenData.Family (GenData) where

import qualified Crypto.PubKey.RSA as RSA

import           Data.Kind

import           Fission.Prelude

type family GenData cipher :: Type

type instance GenData RSA.PrivateKey = ()
