{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Web.Server where

import RIO
import Servant

type RIOServer cfg a = ServerT a (RIO cfg)

-- | Natural transformation `RIO cfg -> Handler`
toHandler :: cfg -> RIO cfg m -> Servant.Handler m
toHandler cfg = liftIO . runRIO cfg
