{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission where

import Fission.Env
import RIO

-- | Top-level application type
type Fission = RIO Env
