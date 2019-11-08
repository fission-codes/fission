-- | Request error type, for log formatting
module Fission.Web.Log.RequestErr.Types where

import RIO

import Data.Aeson
import Network.Wai

import Fission.Internal.Orphanage.Wai.Request ()

data RequestErr = RequestErr
  { request :: !(Maybe Request)
  , error   :: !SomeException
  } deriving Show

instance ToJSON RequestErr where
  toJSON RequestErr { request, error = err } = object
    [ "request" .= toJSON request
    , "error"   .= String (tshow err)
    ]
