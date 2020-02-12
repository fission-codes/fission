module Fission.Web.Auth.Token
  ( get
  , module Fission.Web.Auth.Token.Types
  ) where

import           Fission.Prelude

import qualified RIO.ByteString as BS
import           Network.Wai
import           Fission.Web.Auth.Token.Types

import qualified Fission.Web.Auth.Token.Basic.Types  as Basic
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

get :: Request -> Maybe Token
get req =
  req
    |> requestHeaders
    |> lookup "Authorization"
    |> bind \token ->
         case BS.stripPrefix "Basic " token of
           Just basic' ->
             Just . Basic <| Basic.Token basic'

           Nothing ->
             token
               |> BS.stripPrefix "Bearer "
               |> fmap (Bearer . Bearer.Token)
