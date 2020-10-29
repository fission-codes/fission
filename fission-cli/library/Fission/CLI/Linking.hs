module Fission.CLI.Linking where

{-
Everyone subscribes to channel
Requestor broadcasts public key
Open a secure channel
Provider authentication over UCAN
Confirm requestor PIN
Credential delegation
-}

import           Data.ByteString.Lazy.Char8 as BS8
import qualified RIO.ByteString.Lazy        as Lazy

import           Network.IPFS.Local.Class   as IPFS
import qualified Network.IPFS.Process.Error as IPFS.Process

import           Fission.Prelude

subscribe ::
  ( MonadLocalIPFS m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  )
  => Text
  -> m [BS8.ByteString]
subscribe channel =
  -- NOTE blocks thread, not a feed
  ensureM $ IPFS.runLocal ["pubsub", "sub"] (Lazy.fromStrict $ encodeUtf8 channel)
