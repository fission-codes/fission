module Fission.Web.Auth.Token.UCAN.Resource.Types (Resource (..)) where

import           Fission.Prelude
 
import           Fission.URL
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

-- FIXME write docs about why not a list of these

data Resource
  = -- FIXME add ANY, though probaly at the JWT layer
  FissionFileSystem FilePath
  -- ^ Fission FileSystem path
  | FissionApp        (Scope URL)
  -- ^ Primary URL for an App
  | RegisteredDomain  (Scope DomainName)
  -- ^ Any domain name to which we have DNS access
  deriving (Eq, Show)

instance Arbitrary Resource where
  arbitrary =
    oneof
      [ FissionFileSystem <$> arbitrary
      , FissionApp        <$> arbitrary
      , RegisteredDomain  <$> arbitrary
      ]

instance FromJSON Resource where
  parseJSON = withObject "Resource" \obj -> do
    fs  <- fmap FissionFileSystem <$> obj .:? "fs"
    app <- fmap FissionApp        <$> obj .:? "app_url"
    url <- fmap RegisteredDomain  <$> obj .:? "domain"

    case fs <|> app <|> url of
      Just parsed -> return parsed
      Nothing     -> fail "Does not match any known Fission resource"

instance ToJSON Resource where
  toJSON = \case
    FissionFileSystem path   -> object [ "fs"      .= path   ]
    FissionApp        url    -> object [ "app_url" .= url    ]
    RegisteredDomain  domain -> object [ "domain"  .= domain ]
