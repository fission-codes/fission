module Fission.Internal.Fixture.Web 
  ( fissionURL
  , failureResp
  , failure502
  , failure504
  ) where

import           Fission.Prelude

import           Servant.Client
import           Servant.Client.Core.Request
import           Servant.Client.Internal.HttpClient

import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Version
import qualified RIO.Seq as Seq

fissionURL :: BaseUrl
fissionURL = BaseUrl (Https) "example.com" 443 "/"

failureResp :: Status -> ClientError
failureResp status = mkFailureResponse fissionURL defaultRequest $
  Response 
    { responseStatusCode = status
    , responseHeaders = Seq.empty
    , responseHttpVersion = http11
    , responseBody = ""
    }


failure502 :: ClientError
failure502 = failureResp status502

failure504 :: ClientError
failure504 = failureResp status504

