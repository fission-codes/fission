{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Web.Internal where

import RIO
import Servant

import Fission
import Fission.Config

type FissionServer a = ServerT a Fission

-- | Natural transformation `Fission -> Handler`
toHandler :: Config -> RIO Config m -> Servant.Handler m
toHandler cfg = liftIO . runRIO cfg
