module Fission.WNFS.Subgraph.Types (Subgraph (..)) where

import qualified RIO.Text                                 as Text

import           Network.IPFS.CID.Types

import           Fission.Prelude

import           Fission.Models                           (UserId)

import           Fission.Authorization.PrivilegeFor.Types
import           Fission.User.Username.Types

import           Fission.URL.DomainName.Types
import           Fission.URL.Types

data Subgraph = Subgraph
  { namespace :: DomainName
  , username  :: Username
  , filePath  :: FilePath
  }
  deriving (Show, Eq)

type instance LookupData Subgraph   = (UserId, CID) -- CID here is existing data root

instance Arbitrary Subgraph where
  arbitrary = do
    namespace <- arbitrary -- FIXME may need some more constraint
    username  <- arbitrary
    filePath  <- arbitrary

    return Subgraph {..}

instance PartialOrder Subgraph where
  relationship sgA sgB =
    case (namespaceCheck, usernameCheck, filePathEq, filePathSubset) of
      (True, True, True,  _)     -> Equal
      (True, True, False, True)  -> Descendant
      (True, True, False, False) -> Ancestor
      _                          -> Siblings

  where
    namespaceCheck = namespace sgA == namespace sgB
    usernameCheck  = username  sgA == username  sgB

    filePathEq = filePath sgA == filePath sgB

    filePathSubset =
      Text.isPrefixOf (withEndSlash (filePath sgA)) (withEndSlash (filePath sgB))

    withEndSlash path =
      Text.stripSuffix "/" (Text.pack path) <> "/"

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
