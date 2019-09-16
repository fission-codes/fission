{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fission.Internal.Orphanage.Natural () where

import RIO
import System.Envy

instance Display Natural where
  display nat = display (fromIntegral nat :: Integer)

instance Var Natural where
  fromVar = readMaybe
