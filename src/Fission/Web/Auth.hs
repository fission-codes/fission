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

server :: HasServer api '[BasicAuthCheck Text]
              => Has IpfsPath cfg
              => HasLogFunc cfg
              => Proxy api
              -> cfg
              -> RIOServer cfg api
              -> Server api
server api cfg serv = hoistServerWithContext api context (toHandler cfg) serv

context :: Proxy (BasicAuthCheck Text ': '[])
context = Proxy

basic :: Context (BasicAuthCheck Text ': '[])
basic = BasicAuthCheck check :. EmptyContext

check :: Monad m => BasicAuthData -> m (BasicAuthResult Text)
check (BasicAuthData _username _password) = return $ Authorized ("yup" :: Text)
