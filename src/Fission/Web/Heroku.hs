{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Heroku where

import RIO

import Servant.API

import Fission.Heroku
import Fission.Web.Server

type ProvisionAPI = ReqBody '[JSON] ProvisionReq
      :> Post    '[JSON] ProvisionResp

-----------------------

type APIA = "heroku" :> Capture "x" Int :> "resources" :> Get '[JSON] Text
type APIB = "foo" :> Get '[JSON] Text

type API' = APIA :<|> APIB

api :: Proxy API'
api = Proxy

server :: RIOServer cfg API'
server = heroku :<|> foo

heroku :: RIOServer cfg APIA
heroku _x = return "hi"

foo :: RIOServer cfg APIB
foo = return "hi"
