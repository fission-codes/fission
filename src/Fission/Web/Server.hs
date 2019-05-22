{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Web.Server
  ( RIOServer
  , toHandler
  ) where

import RIO
import Servant (Handler, ServerT)

type RIOServer cfg api = ServerT api (RIO cfg)

-- | Natural transformation `RIO cfg -> Handler`
toHandler :: cfg -> RIO cfg m -> Servant.Handler m
toHandler cfg = liftIO . runRIO cfg
