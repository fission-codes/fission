{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Heroku
  ( API
  , server
  ) where

import RIO

import Servant.API

import qualified Fission.Web.Heroku.Provision as Provision
import           Fission.Web.Server

type API = "resources" :> Provision.API

server :: HasLogFunc cfg => RIOServer cfg API
server = Provision.server
