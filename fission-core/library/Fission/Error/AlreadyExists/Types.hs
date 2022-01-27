module Fission.Error.AlreadyExists.Types (AlreadyExists (..)) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           RIO

import           Web.DID.Types

data AlreadyExists entity
  = AlreadyExists
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (AlreadyExists Ed25519.SecretKey) where
  display _ = "Ed25519 secret key already exists"

instance Display (AlreadyExists DID) where
  display _ = "DID already exists / account already created"
