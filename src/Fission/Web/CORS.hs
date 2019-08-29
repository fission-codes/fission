module Fission.Web.CORS
  ( allowedMethods
  , middleware
  , policy
  ) where

import RIO

import qualified Network.HTTP.Types          as HTTP
import qualified Network.Wai                 as WAI
import           Network.Wai.Middleware.Cors

middleware :: WAI.Middleware
middleware = cors (const $ Just simpleCorsResourcePolicy)

policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy { corsMethods = allowedMethods }

allowedMethods :: [HTTP.Method]
allowedMethods = "PUT" : simpleMethods
