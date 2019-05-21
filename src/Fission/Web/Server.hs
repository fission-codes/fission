{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Web.Server
  ( RIOServer
  , toHandler
  ) where

import RIO
import Servant
import Servant.Client

type RIOServer cfg api = ServerT api (RIO cfg)

-- | Natural transformation `RIO cfg -> Handler`
toHandler :: cfg -> RIO cfg m -> Servant.Handler m
toHandler cfg = liftIO . runRIO cfg

type RIOClient cfg api = RIO cfg (Client IO api) -- TODO Move to Fission.Web.Client
type RIOClient' cfg api = Client (RIO cfg) api -- TODO Move to Fission.Web.Client

-- getClients :: ClientEnv -> Client IO HoistClientAPI
-- getClients clientEnv proxy =
--   hoistClient
--     proxy
--     (fmap (either (error . show) id) . flip runClientM clientEnv) -- NAT TRANS
--     (client hoistClientAPI) -- Source monad
