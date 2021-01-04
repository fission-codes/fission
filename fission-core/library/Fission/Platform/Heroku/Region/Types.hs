module Fission.Platform.Heroku.Region.Types (Region (..)) where

import           Database.Persist.Sql
import           Data.Swagger as Swagger

import qualified RIO.Text as Text

import           Fission.Prelude

-- | Location of Heroku server requesting keys
data Region
  = California
  | Dublin
  | Frankfurt
  | Oregon
  | Singapore
  | Sydney
  | Tokyo
  | Virginia
  deriving ( Show
           , Read
           , Eq
           , Generic
           )

instance PersistField Region where
  toPersistValue = PersistText . Text.pack . show

  fromPersistValue (PersistText str) =
    case str of
      "California" -> pure California
      "Dublin"     -> pure Dublin
      "Frankfurt"  -> pure Frankfurt
      "Oregon"     -> pure Oregon
      "Singapore"  -> pure Singapore
      "Sydney"     -> pure Sydney
      "Tokyo"      -> pure Tokyo
      "Virginia"   -> pure Virginia
      _            -> Left "Not valid Heroku Region value"

  fromPersistValue _ =
    Left "Not valid Heroku Region type"

instance PersistFieldSql Region where
  sqlType _ = SqlString

instance Arbitrary Region where
  arbitrary = elements
    [ California
    , Dublin
    , Frankfurt
    , Oregon
    , Singapore
    , Sydney
    , Tokyo
    , Virginia
    ]

instance ToJSON Region where
  toJSON = String . \case
    California -> "amazon-web-services::us-west-1"
    Dublin     -> "amazon-web-services::eu-west-1"
    Frankfurt  -> "amazon-web-services::eu-central-1"
    Oregon     -> "amazon-web-services::us-west-2"
    Singapore  -> "amazon-web-services::ap-southeast-1"
    Sydney     -> "amazon-web-services::ap-southeast-2"
    Tokyo      -> "amazon-web-services::ap-northeast-1"
    Virginia   -> "amazon-web-services::us-east-1"

instance FromJSON Region where
  parseJSON = withText "Region" \case
    "amazon-web-services::us-west-1"      -> return California
    "amazon-web-services::eu-west-1"      -> return Dublin
    "amazon-web-services::eu-central-1"   -> return Frankfurt
    "amazon-web-services::us-west-2"      -> return Oregon
    "amazon-web-services::ap-southeast-1" -> return Singapore
    "amazon-web-services::ap-southeast-2" -> return Sydney
    "amazon-web-services::ap-northeast-1" -> return Tokyo
    "amazon-web-services::us-east-1"      -> return Virginia
    bad -> fail $ "Invalid region: " <> show bad

instance ToSchema Region where
    declareNamedSchema _ =
      mempty
        |> type_      ?~ SwaggerString
        |> description ?~ "The Heroku hosted region where the user's data is hoted"
        |> example ?~ toJSON Tokyo
        |> NamedSchema (Just "HerokuRegion")
        |> pure
