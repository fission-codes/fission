module Fission.Web.Client.App
  ( Create
  , Update
  ) where

import qualified Fission.Web.App.Create as App.Create
import qualified Fission.Web.App.Update as App.Update

import           Fission.Web.Routes     (AppPrefix)
import           Servant

import qualified Fission.Web.Auth.Types as Auth

type Create
  = AppPrefix
  :> Auth.HigherOrder
  :> App.Create.API

type Update
  = AppPrefix
  :> Auth.HigherOrder
  :> App.Update.API
