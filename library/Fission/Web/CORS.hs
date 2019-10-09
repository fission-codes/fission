module Fission.Web.CORS
  ( allowedMethods
  , allowedRequestHeaders
  , middleware
  , policy
  ) where

import RIO

import           Network.HTTP.Types
import qualified Network.Wai                 as WAI
import           Network.Wai.Middleware.Cors

middleware :: WAI.Middleware
middleware = cors (const $ Just policy)

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
