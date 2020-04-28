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
rawContent = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImN0eSI6IkpXVCIsInVhdiI6IjAuMS4wIn0.eyJhdWQiOiJkaWQ6a2V5OnoxNllGTXpGZjZCeXhzU3VwZTNKeDY0R2pKOTFycEVwTEdhZGYybUJmeXZKc3V2ZVIiLCJleHAiOjI1ODc1NzcxNTIsImlzcyI6ImRpZDprZXk6ejEzVjNTb2cyWWFVS2hkR0NtZ3g5VVp1VzFvMVNoRkpZYzZEdkdZZTdOVHQ2ODlOb0wyUnRwVnM2NVp3ODk5WXJUTjlXdXhkRUVEbTU0WXhXdVFIUXZjS2ZrWndhOEhUZ29rSHhHRFBFbU5MaHZoNjl6VU1FUDR6anVBUlEzVDhiTVV1bWtTTEdweE5lMWJmUVg2MjRlZjQ1R2hXYjNTOUhNM2d2QUo3UWZ0bThpcW5EUVZjeHdLSGpta1Y0aHZlS01UaXg0YlRSaGllVkhpMW9xVTRRQ1Z5NFFQV3BBQXltcHVDUDlkQW9KRnhTUDZUTkJMWTl2UEtMYXpzZzdYY0ZvdjZVdUxXc0VheEo1U29tQ3BEeDE4MW1FZ1cycVR1ZzVvUWJySndFeGJEOUNNZ1hITFZERTJRZ0xvUU1tZ3NyUGV2WDU3ZEg3MTVOWEMydVk2dm8ybVlDelJZNEt1RFJVc3JrdVlDa2V3TDhxMm9LMUJFRFZ2aTNTZzhwYkM5UVlRNW1NaUhmOHV4aUh4VEFtUGVkdjgiLCJwcmYiOm51bGwsInB0eSI6IkFQUEVORCIsInNjcCI6Ii8ifQ"

jsonRSA2048 :: Text
jsonRSA2048 = "Bearer " <> rawContent <> ".X-pwWVW9aZvA02P3MtEwVHpPAQOI89AyzaEnxTtgbi5QIauB3RWhfvQoUymzTt2D-PLTKgL2fXHznzVjwPcpnFHZL6n8KVBGIQR5ZXiO598mRl7oaUTE1icQ0c8VgXemh5t4ncAWkTs2tDs3NmUxhCC9oK0x7RWgQdKJTQ23L-_VRpdYkZKneVSDf8TrF-6muG73la1NQBG6Fk_F_2kKWzVq3njFiZ92Pfb9SLIx5DTLvMnfAiIzraZ8y08NGRW2UdAT595c0pM2jY77oqscCcDe0lA75UnYocwYOdoqbxy4R7q3ENwaNNQNuVzyukrcXDArcidUholYxfZRvczGhQ"

tokenRSA2048 :: Bearer.Token
jwtRSA2048   :: JWT

Right tokenRSA2048@(Bearer.Token jwtRSA2048 _) = parseUrlPiece jsonRSA2048
