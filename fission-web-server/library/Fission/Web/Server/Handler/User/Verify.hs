{-# LANGUAGE AllowAmbiguousTypes #-}
module Fission.Web.Server.Handler.User.Verify (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.Verify.Types as API

handler :: Monad m => ServerT API.Verify m
handler _ = return True
