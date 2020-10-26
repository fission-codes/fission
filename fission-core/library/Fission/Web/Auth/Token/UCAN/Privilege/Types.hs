module Fission.Web.Auth.Token.UCAN.Privilege.Types (Privilege (..)) where

import qualified RIO.Text                       as Text

import           Fission.Prelude

import qualified Fission.App.Privilege.Types    as App
import qualified Fission.Domain.Privilege.Types as Domain
import qualified Fission.WNFS                   as WNFS

-- | Rights to a resource, plus capabilties for it
data Privilege
  = WNFS             WNFS.Privilege   -- ^ Fission FileSystem path
  | FissionWebApp    App.Privilege    -- ^ Primary URL for an App FIXME needs capability
  | RegisteredDomain Domain.Privilege -- ^ Any domain name to which we have DNS access -- FIXME needs capabilty
  | NonFission       Text
  deriving (Eq, Show)

instance Arbitrary Privilege where
  arbitrary =
    oneof
      [ WNFS             <$> arbitrary
      , FissionWebApp    <$> arbitrary
      , RegisteredDomain <$> arbitrary
      , NonFission       <$> arbitrary
      ]

instance PartialOrder Privilege where
  relationship x y =
    case (x, y) of
      (WNFS             a, WNFS             b) -> relationship a b
      (FissionWebApp    a, FissionWebApp    b) -> relationship a b
      (RegisteredDomain a, RegisteredDomain b) -> relationship a b
      (NonFission       a, NonFission       b) -> if a == b then Equal else Sibling
      _                                        -> Sibling

instance FromJSON Privilege where
  parseJSON = withObject "Privilege" \obj -> do
    fs  <- fmap WNFS             <$> obj .:? "wnfs"
    app <- fmap FissionWebApp    <$> obj .:? "web"
    url <- fmap RegisteredDomain <$> obj .:? "domain"

    case fs <|> app <|> url of
      Just parsed -> return parsed
      Nothing     -> return . NonFission . Text.pack $ show obj

instance ToJSON Privilege where
  toJSON = \case
    WNFS             wnfs   -> object [ "wnfs"   .= wnfs   ]
    FissionWebApp    url    -> object [ "app"    .= url    ]
    RegisteredDomain domain -> object [ "domain" .= domain ]
    NonFission       txt    -> String txt
