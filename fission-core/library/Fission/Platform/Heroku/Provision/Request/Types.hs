module Fission.Platform.Heroku.Provision.Request.Types (Request (..)) where

import           Data.UUID as UUID
import           Data.Swagger hiding (name)

import           Fission.Prelude
import qualified Fission.Plan.Types                   as Plan
import qualified Fission.Platform.Heroku.Region.Types as Heroku

data Request = Request
  { callbackUrl :: Text          -- ^ The URL which should be used to retrieve updated information about the add-on and the app which owns it.
  , name        :: Text          -- ^ Logical name of the resource being provisioned.
  -- , oauthGrant :: Maybe Text -- OAuthGrant -- ^ OAuth object details (nullable).
  , plan        :: Plan.Tier     -- ^ Name of the plan to provision (e.g. `basic`).
  , region      :: Heroku.Region -- ^ Physical hosting region of the requesting client.
  , uuid        :: UUID          -- ^ The unique identifier Heroku uses for the installed add-on. It corresponds with the id field in the Heroku Platform API.
  } deriving ( Eq
             , Show
             )

instance Arbitrary Request where
  arbitrary = do
    callbackUrl <- arbitrary
    name        <- arbitrary
    plan        <- arbitrary
    region      <- arbitrary
    uuid        <- arbitrary

    return Request {..}

instance ToJSON Request where
  toJSON Request {..} = object
    [ "callback_url" .= callbackUrl
    , "name"         .= name
    , "plan"         .= plan
    , "region"       .= region
    , "uuid"         .= uuid
    ]

instance FromJSON Request where
  parseJSON = withObject "Heroku.Request" \obj -> do
    callbackUrl <- obj .: "callback_url"
    name        <- obj .: "name"
    plan        <- obj .: "plan"
    region      <- obj .: "region"
    uuid        <- obj .: "uuid"
    return Request {..}

instance ToSchema Request where
  declareNamedSchema _ = do
    planSchema   <- declareSchemaRef <| Proxy @Plan.Tier
    regionSchema <- declareSchemaRef <| Proxy @Heroku.Region
    stringSchema <- declareSchemaRef <| Proxy @String
    uuidSchema   <- declareSchemaRef <| Proxy @UUID

    mempty
      |> example ?~ toJSON Request
                      { callbackUrl = "callback.herokuapp.com/foo"
                      , name        = "my-awesome-app"
                      , plan        = Plan.Free
                      , region      = Heroku.Tokyo
                      , uuid        = fromJust <| UUID.fromString "0cebfcfe-93c9-11e9-bc42-526af7764f64"
                      }
      |> required .~ [ "callbackUrl"
                     , "name"
                     , "plan"
                     , "region"
                     , "uuid"
                     ]
      |> properties .~ [ ("callbackUrl", stringSchema)
                             , ("name",        stringSchema)
                             , ("plan",        planSchema)
                             , ("region",      regionSchema)
                             , ("uuid",        uuidSchema)
                             ]
      |> description ?~ "Request from Heroku to provision a new user"
      |> title       ?~ "Heroku Provisioning Request"
      |> type_       ?~ SwaggerObject
      |> NamedSchema (Just "ProvisionRequest")
      |> pure
