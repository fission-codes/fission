module Fission.WNFS.Subgraph.Types (Subgraph (..)) where

import           Data.Bits

import qualified RIO.Text                                     as Text

import           Network.IPFS.CID.Types

import           Fission.Prelude

import           Fission.Models

import           Fission.User.Username.Types                  (Username)

import           Fission.URL.DomainName.Types
import           Fission.URL.Types

import           Fission.Authorization.Access.Unchecked.Types

data Subgraph = Subgraph
  { namespace :: !DomainName
  , username  :: !Username
  , filePath  :: !FilePath
  }
  deriving (Show, Eq)

instance Arbitrary Subgraph where
  arbitrary = do
    let namespace = "fission.name"

    username <- arbitrary
    filePath <- arbitrary

    return Subgraph {..}

instance PartialOrder Subgraph where
  relationship sgA sgB =
    case (namespaceCheck, usernameCheck) of
      (True, True) ->
        if "/private/" `Text.isPrefixOf` pathA
          then checkPrivate
          else checkPublic

      _ ->
        Sibling

    where
      checkPrivate =
        case (pathA == pathB, bytesA `containedIn` bytesB, bytesB `containedIn` bytesA) of
          (True, _, _)    -> Equal
          (_, True, _)    -> Descendant
          (_, _,    True) -> Ancestor
          _               -> Sibling

      xs `containedIn` ys = zipWith (.|.) xs ys == ys

      checkPublic =
        case (pathA == pathB, pathSubset pathA pathB, pathSubset pathB pathA) of
          (True, _,    _)   -> Equal
          (_,    True, _)   -> Descendant
          (_,    _,   True) -> Ancestor
          _                 -> Sibling

      namespaceCheck = namespace sgA == namespace sgB
      usernameCheck  = username  sgA == username  sgB

      pathA = Text.pack $ filePath sgA
      pathB = Text.pack $ filePath sgB

      bytesA = ord <$> filePath sgA
      bytesB = ord <$> filePath sgB

      pathSubset x y = withEndSlash x `Text.isPrefixOf` withEndSlash y

      withEndSlash path = Text.dropSuffix "/" path <> "/"

instance ToJSON Subgraph where
  toJSON Subgraph {..} =
    String (textDisplay username <> "." <> textDisplay namespace <> normalizedPath)
    where
      txt =
        Text.pack filePath

      normalizedPath =
        case Text.uncons txt of
         Just ('/', _) -> txt
         _             -> "/" <> txt

instance FromJSON Subgraph where
  parseJSON = withText "WNFS.Subgraph" \txt -> do
    let
      (url, path') = Text.break (/= '/') txt
      pathStr      = Text.unpack path'

      filePath =
        case pathStr of
         ('/' : _) -> pathStr
         _         -> '/' : pathStr

    URL
      { subdomain = Just (Subdomain rawSubdomain)
      , domainName
      } <- parseJSON $ String url

    username <- parseJSON $ String rawSubdomain

    return Subgraph
      { namespace = domainName
      , username
      , filePath
      }
