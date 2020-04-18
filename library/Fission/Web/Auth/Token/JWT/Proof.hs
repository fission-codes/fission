module Fission.Web.Auth.Token.JWT.Proof
  ( delegatedInBounds
  , signaturesMatch
  , scopeInSubset
  , potencyInSubset

  -- * Reexport

  , module Fission.Web.Auth.Token.JWT.Proof.Error
  ) where

import qualified RIO.Text as Text

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT as JWT
import           Fission.Web.Auth.Token.JWT.Proof.Error

delegatedInBounds :: JWT -> JWT -> Either Error JWT
delegatedInBounds jwt prfJWT = do
  signaturesMatch jwt prfJWT
  scopeInSubset   jwt prfJWT
  potencyInSubset jwt prfJWT

signaturesMatch :: JWT -> JWT -> Either Error JWT
signaturesMatch jwt prfJWT =
  if (jwt |> claims |> sender) == (prfJWT |> claims |> receiver)
    then Right jwt
    else Left InvalidSignatureChain -- $ Nested raw

scopeInSubset :: JWT -> JWT -> Either Error JWT
scopeInSubset jwt prfJWT =
  case Text.stripPrefix (jwt |> claims |> scope) (prfJWT |> claims |> scope) of
    Just _  -> Right jwt
    Nothing -> Left ScopeOutOfBounds -- $ Nested raw

potencyInSubset :: JWT -> JWT -> Either Error JWT
potencyInSubset jwt prfJWT =
  if (jwt |> claims |> potency) <= (prfJWT |> claims |> potency)
    then Right jwt
    else Left PotencyEscelation
