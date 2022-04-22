{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Web.UCAN.Orphanage.DummyResolver where

import           Test.Web.UCAN.Prelude
import           Web.UCAN.Resolver

instance Resolver IO where
  resolve _ = return $ Left undefined
