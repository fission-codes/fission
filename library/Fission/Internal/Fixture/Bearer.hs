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
rawContent = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsInVhdiI6IjAuMS4wIn0.eyJpc3MiOiJkaWQ6a2V5OnoxUnRiVWlBdTM3ZnF3blNxa1d4YXZCNHNvMXd3aEo2bmFKaVU2cWZTd2t6bXNwRUJVODFCeVVnQWY2b0x4YktzZ3RXSjRuZ0NTUFJyajM1TEdmNzhIdVBxWmQ5cDVUMnNXMzRBWVpVN1lkTGJyZmk2d1lDY1FvanJScmJVQUV0SmVKR2pEcDhOcWV6ZGFmRXA1Y2JuZUdVN1NNQ3RYZktHZmpUOTYzOEFXOURYU0xQZ0RHdVZoaU00TVVKVXFYQ2JHeDV5VWVLdVhXTDJ6dmNvQUE5TEF4R2c3VjJwOGdZRDZyZGpIc3ZYa2NBblQ0dDJQS1czd2Fqakp2Y2k4blR4NlhDcTdyb0NVVlU4QkNyWm9XdUhrclVQc1NHNVdCeW9NZHN2ZHVTMnoxSjhLMXFvdTl6VFkyU2l5bXg4aVp1RXBvUU1KSjRqRDlxeXp1VlBmOUd5ZW43MWdTTTJWZ3l0SExpak5rNyIsImF1ZCI6ImRpZDprZXk6ekJSNG0zRE5aSFQxRzhOYjJSSHpnS0s3VHJXeEVtSmpac2tndkZkbmNUdGh6VUh6bmd5TkttS3g0VktXRUpFNnNrNFNFNEthM2tIOTJNeFUyWUM3Q2NlUEh5NzdHelp5OCIsInNjcCI6Ii8iLCJwdHkiOiJBUFBFTkQiLCJleHAiOjI1MTYyMzkwMjJ9"

jsonRSA2048 :: Text
jsonRSA2048 = "Bearer " <> rawContent <> ".XRTWq7WO5ZRne1o7pFeSiaxyBOTv1q-FRjE2GyIRnWMwEqZg5jUqhxzGYhmhBlorv9rF8RC4mBVkoo7ZkYozNh8CFuwQTQeJ-z2VM5N-C03JDgmUrVqbzEwr0N4mIhj6Ibf2_FwsPaURTe9vYivJK0pe3HyJ2r8GOVOAtQOCh1D8hoKE8KyWpTrAvFIfIfzwr8rqdStHD2HhN0LERK7vdHS-ETiLa8v-AaOIyt_rJMYXDp9EkMbWrrgi8gKhvZrHIP3e0PRxQNjEmmstWw69PHCrfITgAyUMySYauc01yEXKan1tEqscubf0UgX62OLD3sWh6odg1Mcq9-G6YE5ueg"

tokenRSA2048 :: Bearer.Token
jwtRSA2048   :: JWT

Right tokenRSA2048@(Bearer.Token jwtRSA2048 _) = parseUrlPiece jsonRSA2048
