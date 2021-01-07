{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Fission.Web.Client.DNS (set) where

import           Fission.Web.API.Prelude   hiding (set)

import           Fission.Web.API.DNS.Types

set = client $ Proxy @DNS
