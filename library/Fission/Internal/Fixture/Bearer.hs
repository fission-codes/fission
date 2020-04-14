{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fission.Internal.Fixture.Bearer
  ( jsonRSA2048
  , tokenRSA2048
  , jwtRSA2048
  , validTime
  ) where

import           Servant.API

import           Fission.Prelude
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.JWT.Types

validTime :: UTCTime
validTime = fromSeconds 0 -- i.e. waaaay before the expiry :P

-- NOTE this has a PSS signature. We need PKCS for JWT w/ RS256. Waiting on the FE team to maek that change.
jsonRSA2048 :: Text
jsonRSA2048 = "Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE1ODYzNzEzNDkxMjYsImlzcyI6ImRpZDprZXk6ejEzVjNTb2cyWWFVS2hkR0NtZ3g5VVp1VzFvMVNoRkpZYzZEdkdZZTdOVHQ2ODlOb0wzZkF2TlhDQkhNbnVab2JVVm9yVGtQZ1FBbVl3TmdQS1pGZVp0Z0Z0OE1nUWNweGFXVFV4NjVocUtmcHVSd3JqZnFHSlRMbmk1dnoxZzg1RWlDdUhLeW9XMmp1cW54VXRIOUZkV0JVR0VwVWlvTk1CNFZydk1GWlE4VHpSQmJ1SlZyS3pFMURhVWdvYTdqcnM5bU1Ubmh1Y1h4NW1uOWE0aVlEU0xTZHI2bzVuckQ4c0VEbllmdWNYc0w4dHVNQjlhaUhjYWlidE1Ya3oxUmlXRXNndzhTOTliWjgxQmNuM3l2ZTJHOUtrU3F0WHNlNmZTaXJXaTFyS2hUMUxSQ0VCSnhYbkFxY25NZlQ0cU5ORE5USlUzb2VWUmVWYzJ6bXhhZUM4aGNIWXAzRjdnNWtjclJONEdvc2lQdnpWVkdhNXZDTnBrNHc4OHJkTVZiek5uanJVWlNhRmY3RVRmWmhRd25MQ0ciLCJuYmYiOm51bGx9.kHGfUlB-pog86ON5loYsLGPydcRvxk49ez-AI17ko6Bj8OBdHqnSwggHl3NMy9DCEg4N0Xkmp2X_-E4gnlJ9hCNEDJB2v61sbQCvqVJZtrF2RGAk8ZyCQLA5dHuRTvb8jhI453C-_ocnkH0Q-42Z8jI_JkeghauRqJanudBJQOcX2MbRrt84zYZ7eATRM-sXUdy2NtaGPwP2i5h2iNOZGlSnj-GsnFkzIBSPHBw2Wm1p-1MJ2DM_smWx_b_NfE379KsvDIRPZ0y_3ZPO3tCWAi9t69jls1_RKD5EdwkKFTnGbCWaHLdF-78-YvTJcQ2fsj4e5xD-Ok6shQvrkkLt5A"

tokenRSA2048 :: Bearer.Token
jwtRSA2048   :: JWT

Right tokenRSA2048@(Bearer.Token jwtRSA2048) = parseUrlPiece jsonRSA2048
