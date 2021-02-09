module Fission.Web.Auth.Token.UCAN.Resource.Types (Resource (..)) where

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import           Fission.URL
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

data Resource
  = FissionFileSystem FilePath
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
