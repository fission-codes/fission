{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.OpenUnion () where

import RIO
import Data.WorldPeace

instance Display (OpenUnion '[]) where
  -- As a base case for the type-level recursion only. Essentially @Void@.
  display = absurdUnion

instance (Display a, Display (OpenUnion as)) => Display (OpenUnion (a ': as)) where
  display err = openUnion display display err

instance Exception (OpenUnion '[]) where
  toException = absurdUnion

instance (Typeable errs, Exception err, Exception (OpenUnion errs))
  => Exception (OpenUnion (err ': errs)) where
    toException err = openUnion toException toException err
