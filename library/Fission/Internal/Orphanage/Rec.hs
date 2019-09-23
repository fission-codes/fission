{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage.Rec () where

import RIO
import RIO.Process (ProcessContext, HasProcessContext (..))

import SuperRecord as SR

instance Has "logFunc" cfg LogFunc => HasLogFunc (Rec cfg) where
  logFuncL = SR.lens #logFunc

instance Has "processCtx" cfg ProcessContext => HasProcessContext (Rec cfg) where
  processContextL = SR.lens #processCtx
