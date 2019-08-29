module Fission.Web.CORS
  ( allowedMethods
  , middleware
  , policy
  ) where

import RIO

import           Network.HTTP.Types
import qualified Network.Wai                 as WAI
import           Network.Wai.Middleware.Cors

middleware :: WAI.Middleware
middleware = cors (const $ Just simpleCorsResourcePolicy)

policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy { corsMethods = allowedMethods }

allowedMethods :: [Method]
allowedMethods = [ methodGet
                 , methodDelete
                 , methodHead
                 , methodOptions
                 , methodPatch
                 , methodPost
                 , methodPut
                 ]
