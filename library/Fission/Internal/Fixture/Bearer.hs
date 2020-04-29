{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fission.Internal.Fixture.Bearer
  ( jsonRSA2048
  , tokenRSA2048
  , jwtRSA2048
  , rawContent
  , validTime
  ) where

import           Servant.API

import           Fission.Prelude
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT

validTime :: UTCTime
validTime = fromSeconds 0 -- i.e. waaaay before the expiry :P

rawContent :: Text
rawContent = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsInVhdiI6IjAuMS4wIiwiY3R5IjoiSldUIn0.eyJhdWQiOiJkaWQ6a2V5OnpTdEVacHpTTXRUdDlrMnZzemd2Q3dGNGZMUVFTeUExNVc1QVE0ejNBUjZCeDRlRko1Y3JKRmJ1R3hLbWJtYTQiLCJleHAiOjI1ODc1NzcxNTIsImlzcyI6ImRpZDprZXk6ejFSaTdSeWRDenMxVDNVckZOc1ZyenBSMXhIOGI1TW9ZRTJCSlN3RERMQ2RoQ3daZGpOU25BMzhKM3g2Q1oza0Rwem1nNWVLcmF5dVNCRnYzQ3VRQzRESldlVjNiYmJUUGFRcTh4Wk56c1Z1VWl0VmVwSFhXa2JKSHoyRHJBc1EyeHpWcTFDU2ZOQTRNVVNKaXNlMXR4UkFERlJiZVFTamRFWDFYajZ6enVCY3J5VVpjaFhuVjdlM0haQmJoVzlHU3hMUWtIZzU4SGRpZkZZUXZBa0Noakd6YldyQ0NLWFFKM2VhS29xS2FZUU5EbUxBOENkdEJja0x3b05Vald1eGl3TlhvcXo0Z3BBa1doZ0xEZ0Y5bXhrcGRKSkVGRlc1WW41ZHlwSFNjbm9nN0xkWm53NVlRaFFEVGhjMWtHdVpWQVhuUUM5dlZ0TnpYUGp6N29oR0hqNm4xcUZrOHM5M3M5VHd6SGZFMmt6RlB0YVNQZmlvRUxvY0JNaUJHZmdmcFNkSmtzNW5aU0c4M3hkUHo0V0U4YjVLRkJXWnRIblRzV0N5d3B2TnpBTU5lbW12WW0yZU02Q2ZQYWE2RVUxMXVVbmlZOW81R2JuVWdDbTFrYm1qZHBKbjFZajNLSHBpRlBocW1RMnlSeEJFQ2tWWlU4UEc3UGY2WTVuMlp0MU13YUE3QUdhWVdSNE50M1dCQ2Z1M0dvckhHam05c1E1IiwicHJmIjpudWxsLCJwdHkiOiJBUFBFTkQiLCJzY3AiOiIvIn0"

jsonRSA2048 :: Text
jsonRSA2048 = "Bearer " <> rawContent <> ".UlWoyJ_QtDUBnoPyIeY-NBmqNF_By28G_SuHAzhOKcR3d1E1NCFxNBWGOBcn2ik26MZBK1MNd9WfGrrxPBDFE-xXIE8ETL02XzLMZQjJD2AROOmAT0FPg9xA1_OVSFtp5NWBXS69pPYLaBDgP_qXvYlecJmwZxXV00A--eiolFaBNzvl-Qoq6E7tLTRZPxNoWGeAn-wqLa42S2v2MUt7pDRU9qR04vXDSzd9N2A9MCii2CK2mcAOTadVB3uD6saDWuQdZEHRDlndf8-wvBl8xZD3ixS-36XMRJ3NSC9wVdbn30nhXWOnmJY6amyfZiuzKTMr_CO3-N-lYLQfqH0L7Q"

tokenRSA2048 :: Bearer.Token
jwtRSA2048   :: JWT

Right tokenRSA2048@(Bearer.Token jwtRSA2048 _) = parseUrlPiece jsonRSA2048
