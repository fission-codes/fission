module Fission.Web.Auth.Token.JWT.Proof
  ( delegatedInBounds
  , signaturesMatch
  , resourceInSubset
  , potencyInSubset
  , containsFact

  -- * Reexport

  , module Fission.Web.Auth.Token.JWT.Proof.Error
  ) where

import qualified RIO.ByteString                                   as BS
import qualified RIO.List                                         as List
import qualified RIO.Text                                         as Text

import qualified Data.Bits                                        as Bits

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT.Fact.Types
import           Fission.Web.Auth.Token.JWT.Proof.Error
import           Fission.Web.Auth.Token.JWT.Types                 as JWT

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

delegatedInBounds :: JWT -> JWT -> Either Error JWT
delegatedInBounds  jwt prfJWT = do
  signaturesMatch  jwt prfJWT
  resourceInSubset jwt prfJWT
  potencyInSubset  jwt prfJWT
  timeInSubset     jwt prfJWT

signaturesMatch :: JWT -> JWT -> Either Error JWT
signaturesMatch jwt prfJWT =
  if (jwt |> claims |> sender) == (prfJWT |> claims |> receiver)
    then Right jwt
    else Left InvalidSignatureChain

resourceInSubset :: JWT -> JWT -> Either Error JWT
resourceInSubset jwt prfJWT =
  case ((jwt |> claims |> resource), (prfJWT |> claims |> resource)) of
    (Nothing,           _)                      -> Right jwt
    (_,                 Just Complete)          -> Right jwt
    (Just (Subset rsc), Just (Subset rscProof)) -> compareSubsets rsc rscProof
    _                                           -> Left ScopeOutOfBounds

  where
    compareSubsets :: Resource -> Resource -> Either Error JWT
    compareSubsets rsc rscProof =
      case (rsc, rscProof) of
        (FissionFileSystem path, FissionFileSystem proofPath) ->
          -- NOTE `List` because FilePath ~ String ~ [Char]
          case (isPrivate path, isPrivate proofPath, pathSubset path proofPath) of
            (True, True, _) ->
              if bitwiseContains proofPath path
                then Right jwt
                else Left ScopeOutOfBounds

            (_, _, True) ->
              Right jwt

            _ ->
              Left ScopeOutOfBounds

        (RegisteredDomain _, FissionApp Complete) ->
          Right jwt

        (FissionApp _, FissionApp Complete) ->
          Right jwt

        _ ->
          if rsc == rscProof
            then Right jwt
            else Left ScopeOutOfBounds

    isPrivate path = "/private/" `Text.isPrefixOf` path

    bitwiseContains :: Text -> Text -> Bool
    path `bitwiseContains` proof =
      all (== True) $ List.zipWith containByOR pathWords proofWords
      where
        containByOR :: Word8 -> Word8 -> Bool
        containByOR pathChunk proofChunk = pathChunk Bits..|. proofChunk == pathChunk

        pathWords  = List.take longest $ pathWords'  ++ padding
        proofWords = List.take longest $ proofWords' ++ padding

        longest = max (List.length pathWords') (List.length proofWords')
        padding = List.repeat 0

        pathWords'  = BS.unpack $ encodeUtf8 path
        proofWords' = BS.unpack $ encodeUtf8 proof

    pathSubset inner outer = normalizePath outer `Text.isPrefixOf` normalizePath inner

    normalizePath path =
      if "/" `Text.isSuffixOf` path
        then path
        else path <> "/"

potencyInSubset :: JWT -> JWT -> Either Error JWT
potencyInSubset jwt prfJWT =
  if (jwt |> claims |> potency) <= (prfJWT |> claims |> potency)
    then Right jwt
    else Left PotencyEscelation

timeInSubset :: JWT -> JWT -> Either Error JWT
timeInSubset jwt prfJWT =
  if startBoundry && expiryBoundry
    then Right jwt
    else Left TimeNotSubset

  where
    startBoundry  = (jwt |> claims |> nbf) >= (prfJWT |> claims |> nbf)
    expiryBoundry = (jwt |> claims |> exp) <= (prfJWT |> claims |> exp)

containsFact :: JWT -> ([Fact] -> Either Error ()) -> Either Error JWT
containsFact jwt factChecker =
  jwt
    |> claims
    |> facts
    |> factChecker
    |> \case
        Left err -> Left err
        Right () -> Right jwt
