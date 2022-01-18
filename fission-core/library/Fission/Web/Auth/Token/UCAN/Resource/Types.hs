module Fission.Web.Auth.Token.UCAN.Resource.Types (Resource (..)) where

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import           Fission.URL
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import qualified RIO.Text as Text
import qualified Data.Bits                                        as Bits
import qualified RIO.ByteString                                   as BS
import qualified RIO.List                                         as List

import Web.JWT.Proof.Class

data Resource
  = FissionFileSystem Text -- More efficent than FilePath
  -- ^ Fission FileSystem path
  | FissionApp        (Scope URL)
  -- ^ Primary URL for an App
  | RegisteredDomain  (Scope DomainName)
  -- ^ Any domain name to which we have DNS access
  deriving (Eq, Ord, Show)

instance Arbitrary Resource where
  arbitrary =
    oneof
      [ FissionFileSystem <$> arbitrary
      , FissionApp        <$> arbitrary
      , RegisteredDomain  <$> arbitrary
      ]

instance Display Resource where
  textDisplay = \case
    FissionFileSystem path            -> "WNFS at "     <> path

    FissionApp        Complete        -> "all apps"
    FissionApp        (Subset url)    -> "app at "      <> textDisplay url

    RegisteredDomain  Complete        -> "all domains names"
    RegisteredDomain  (Subset domain) -> "domain name " <> textDisplay domain

instance ToJSON Resource where
  toJSON = \case
    FissionFileSystem path   -> object [ "wnfs"   .= path   ]
    FissionApp        url    -> object [ "app"    .= url    ]
    RegisteredDomain  domain -> object [ "domain" .= domain ]

instance FromJSON Resource where
  parseJSON = withObject "Resource" \obj -> do
    wnfs   <- fmap FissionFileSystem <$> obj .:? "wnfs"
    floofs <- fmap FissionFileSystem <$> obj .:? "floofs" -- keep around floofs for backward-compatibility
    app    <- fmap FissionApp        <$> obj .:? "app"
    url    <- fmap RegisteredDomain  <$> obj .:? "domain"

    case wnfs <|> floofs <|> app <|> url of
      Just parsed -> return parsed
      Nothing     -> fail "Does not match any known Fission resource"

instance Display (NotFound Resource) where
  display _ = "No UCAN resource provided (closed UCAN)"

instance ResourceSemantics Resource where
  canDelegate rsc rscProof =
    case (rsc, rscProof) of
      (FissionFileSystem path, FissionFileSystem proofPath) ->
        case (isPrivate path, isPrivate proofPath, pathSubset path proofPath) of
          (True, True, _) ->
            bitwiseContains proofPath path

          (_, _, True) ->
            True

          _ ->
            False

      (RegisteredDomain _, RegisteredDomain Complete) ->
        True

      (FissionApp _, FissionApp Complete) ->
        True

      _ ->
        rsc == rscProof

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
