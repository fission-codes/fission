module Fission.Web.Auth.Token.JWT.Proof.WNFS
  ( compareSubsets
  , pathSubset
  , isPrivate
  , bitwiseContains
  , normalizePath
  ) where

import qualified RIO.ByteString                                   as BS
import qualified RIO.List                                         as List
import qualified RIO.Text                                         as Text

import qualified Data.Bits                                        as Bits

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT.Proof.Error
import           Fission.Web.Auth.Token.JWT.Types                 as JWT

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

compareSubsets :: JWT -> Resource -> Resource -> Either Error JWT
compareSubsets jwt rsc rscProof =
  case (rsc, rscProof) of
    (FissionFileSystem path, FissionFileSystem proofPath) ->
      case (isPrivate path, isPrivate proofPath, pathSubset path proofPath) of
        (True, True, _) ->
          if bitwiseContains proofPath path
            then Right jwt
            else Left ScopeOutOfBounds

        (_, _, True) ->
          Right jwt

        _ ->
          Left ScopeOutOfBounds

    (RegisteredDomain _, RegisteredDomain Complete) ->
      Right jwt

    (FissionApp _, FissionApp Complete) ->
      Right jwt

    _ ->
      if rsc == rscProof
        then Right jwt
        else Left ScopeOutOfBounds

isPrivate :: Text -> Bool
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

pathSubset :: Text -> Text -> Bool
pathSubset inner outer = normalizePath outer `Text.isPrefixOf` normalizePath inner

normalizePath :: Text -> Text
normalizePath path =
  if "/" `Text.isSuffixOf` path
    then path
    else path <> "/"
