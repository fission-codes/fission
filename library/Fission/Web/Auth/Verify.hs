module Fission.Web.Auth.Verify
  ( API
  , verify
  ) where

import RIO

import Servant
-- import Servant.API

import Fission.Web.Server

type Head = Verb 'HEAD 200

type API = Head '[JSON] ()

verify :: RIOServer cfg API
verify = pure ()
