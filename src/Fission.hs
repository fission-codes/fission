{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission where

import Fission.Config
import RIO

-- | Top-level application type
type Fission = RIO Config
