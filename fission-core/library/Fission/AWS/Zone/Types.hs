module Fission.AWS.Zone.Types (ZoneID (..)) where

import           Data.Swagger as Swagger
import           Database.Persist.Sql
import           Servant.API

import           Fission.Prelude

-- | Type safety wrapper for a Route53 zone ID
newtype ZoneID = ZoneID { getZoneID :: Text }
  deriving (Eq, Show)

instance Arbitrary ZoneID where
  arbitrary = ZoneID <$> arbitrary

instance Display ZoneID where
  textDisplay = getZoneID

instance PersistField ZoneID where
  toPersistValue (ZoneID txt) = PersistText txt

  fromPersistValue = \case
    PersistText txt -> Right $ ZoneID txt
    bad -> Left $ "ZoneID must be PersistText, but got: " <> toUrlPiece bad

instance PersistFieldSql ZoneID where
  sqlType _ = SqlString

instance Swagger.ToSchema ZoneID where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerString
      |> NamedSchema (Just "AWS.ZoneId")
      |> pure

instance FromJSON ZoneID where
  parseJSON = withText "AWS.ZoneID" \txt ->
    ZoneID <$> parseJSON (String txt)
