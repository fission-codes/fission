{-# LANGUAGE UndecidableInstances #-}

module Fission.Error.NotFound.Types (NotFound (..)) where

import           RIO.FilePath

import           Crypto.Cipher.AES                          (AES256)
import qualified Crypto.PubKey.Ed25519                      as Ed25519

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Types                         as IPFS

import           Fission.Prelude
import           Fission.URL

import           Fission.Key.Symmetric.Types

import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Types

import           Web.DID.Types

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

instance Display (NotFound UCAN) where
  display _ = "Unable to find UCAN"

instance Display (NotFound (Key AES256)) where
  display _ = "Unable to find AES256 key"

instance Display (NotFound Resource) where
  display _ = "No UCAN resource provided (closed UCAN)"
