module Fission.Web.Auth.Token.UCAN.Privilege.Types (Privilege (..)) where

import           Fission.Prelude

import           Fission.URL
import qualified Fission.WNFS    as WNFS

-- | Rights to a resource, plus capabilties for it
data Privilege
  = WNFS             WNFS.Privilege -- ^ Fission FileSystem path
  | FissionWebApp    URL            -- ^ Primary URL for an App FIXME needs capability
  | RegisteredDomain DomainName     -- ^ Any domain name to which we have DNS access -- FIXME needs capabilty
  deriving (Eq, Show)

instance Arbitrary Privilege where
  arbitrary =
    oneof
      [ WNFS             <$> arbitrary
      , FissionWebApp    <$> arbitrary
      , RegisteredDomain <$> arbitrary
      ]

instance FromJSON Privilege where
  parseJSON = withObject "Privilege" \obj -> do
    fs  <- fmap WNFS             <$> obj .:? "wnfs"
    app <- fmap FissionWebApp    <$> obj .:? "web"
    url <- fmap RegisteredDomain <$> obj .:? "domain"

    case fs <|> app <|> url of
      Just parsed -> return parsed
      Nothing     -> fail "Does not match any known Fission resource"

instance ToJSON Privilege where
  toJSON = \case
    WNFS             wnfs   -> object [ "wnfs"   .= wnfs   ]
    FissionWebApp    url    -> object [ "app"    .= url    ]
    RegisteredDomain domain -> object [ "domain" .= domain ]
