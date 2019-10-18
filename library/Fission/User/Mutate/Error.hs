module Fission.User.Mutate.Error (Create (..)) where

import RIO

import Data.Aeson
import Servant.Server

import Fission.Web.Error

data Create = FailedDigest
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Create where
  display FailedDigest = "Could not create password digest"

instance ToServerError Create where
  toServerError FailedDigest = err500 { errBody = "Could not create password digest" }
