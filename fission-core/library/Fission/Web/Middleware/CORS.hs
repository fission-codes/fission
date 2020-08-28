module Fission.Web.Middleware.CORS
  ( allowedMethods
  , allowedRequestHeaders
  , middleware
  , policy
  ) where

import           Network.HTTP.Types
import qualified Network.Wai                 as WAI
import           Network.Wai.Middleware.Cors

import Fission.Prelude

middleware :: WAI.Middleware
middleware = cors (\_ -> Just policy)

policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy
  { corsMethods        = allowedMethods
  , corsRequestHeaders = allowedRequestHeaders
  }

allowedMethods :: [Method]
allowedMethods = [ methodGet
                 , methodDelete
                 , methodHead
                 , methodOptions
                 , methodPatch
                 , methodPost
                 , methodPut
                 ]

allowedRequestHeaders :: [HeaderName]
allowedRequestHeaders = [ hAuthorization
                        , hContentEncoding
                        , hContentType
                        ]
