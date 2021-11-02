{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Network.IPFS.Internal.Orphanage.Natural () where

import System.Envy

import Network.IPFS.Prelude

instance Display Natural where
  display nat = display (fromIntegral nat :: Integer)

instance Var Natural where
  fromVar = readMaybe
