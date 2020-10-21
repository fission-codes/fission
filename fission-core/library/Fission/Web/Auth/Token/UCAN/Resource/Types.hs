module Fission.Web.Auth.Token.UCAN.Resource.Types (Resource (..)) where

import           Fission.Prelude

import           Fission.URL
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

data Resource
  = FissionFileSystem FilePath
  -- ^ Fission FileSystem path
  | FissionWebApp     (Scope URL)
  -- ^ Primary URL for an App
  | RegisteredDomain  (Scope DomainName)
  -- ^ Any domain name to which we have DNS access
  deriving (Eq, Show)

instance Arbitrary Resource where
  arbitrary =
    oneof
      [ FissionFileSystem <$> arbitrary
      , FissionWebApp     <$> arbitrary
      , RegisteredDomain  <$> arbitrary
      ]

instance FromJSON Resource where
  parseJSON = withObject "Resource" \obj -> do
    fs  <- fmap FissionFileSystem <$> obj .:? "wnfs"
    app <- fmap FissionWebApp     <$> obj .:? "web"
    url <- fmap RegisteredDomain  <$> obj .:? "domain"

    case fs <|> app <|> url of
      Just parsed -> return parsed
      Nothing     -> fail "Does not match any known Fission resource"

instance ToJSON Resource where
  toJSON = \case
    FissionFileSystem path   -> object [ "floofs" .= path   ]
    FissionWebApp     url    -> object [ "app"    .= url    ]
    RegisteredDomain  domain -> object [ "domain" .= domain ]
