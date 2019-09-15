module Fission.Web.Client
  ( module Fission.Web.Client.Request
  , module Fission.Web.Client.Types
  ) where

import RIO

import Servant.Client

import Fission.Web.Client.Request
import Fission.Web.Client.Types

import Fission.Web.Routes

-- x = client (Proxy :: Proxy API)
