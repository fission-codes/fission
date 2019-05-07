{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Web.Types where

import RIO
import Servant

import Fission
import Fission.Env

type FissionServer a = ServerT a Fission

-- | Natural transformation `Fission -> Handler`
toHandler :: Env -> RIO Env m -> Servant.Handler m
toHandler env = liftIO . runRIO env
