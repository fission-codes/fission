{-# LANGUAGE UndecidableInstances #-}

module Fission.Error.NotFound.Types (NotFound (..)) where

import           RIO.FilePath

import qualified Crypto.PubKey.Ed25519  as Ed25519

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Types     as IPFS

import           Fission.Prelude
import           Fission.URL
import           Fission.User.DID.Types

data NotFound entity
  = NotFound
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (NotFound DID) where
  display _ = "Unable to find DID"

instance Display (NotFound URL) where
  display _ = "URL not found in system"

instance Display (NotFound FilePath) where
  display _ = "Cannot find file"

instance Display (NotFound [IPFS.Peer]) where
  display _ = "Unable to find IPFS peers"

instance Display (NotFound Ed25519.SecretKey) where
  display _ = "Unable to find Ed25519 secret key"

instance Display (NotFound CID) where
  display _ = "Unable to find CID"
