{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Auth where

import RIO

import Servant

import Data.Has

import Fission.Config
import Fission.Web.Internal

protectedServer :: HasServer api '[BasicAuthCheck Text]
              => Has IpfsPath cfg
              => HasLogFunc cfg
              => Proxy api
              -> cfg
              -> RIOServer cfg api
              -> Server api
protectedServer api cfg serv = hoistServerWithContext api context (toHandler cfg) serv

context :: Proxy (BasicAuthCheck Text ': '[])
context = Proxy

basicAuthContext :: Context (BasicAuthCheck Text ': '[])
basicAuthContext = checkText :. EmptyContext

checkText :: BasicAuthCheck Text
checkText =
  let
    check (BasicAuthData _username _password) = return $ Authorized ("yup" :: Text)
  in
    BasicAuthCheck check
