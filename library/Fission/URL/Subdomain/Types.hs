module Fission.URL.Subdomain.Types (Subdomain (..)) where

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Database.Persist.Postgresql hiding (get)
import           Data.Swagger                hiding (get)

import           Servant

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8

-- | Type safety wrapper for subdomains
newtype Subdomain = Subdomain { get :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving newtype  ( IsString )

instance PersistField Subdomain where
  toPersistValue (Subdomain name') = PersistText name'
  fromPersistValue = \case
    PersistText name' -> Right (Subdomain name')
    other             -> Left ("Invalid Persistent Domain Name: " <> Text.pack (show other))

instance PersistFieldSql Subdomain where
  sqlType _pxy = SqlString

instance ToSchema Subdomain where
  declareNamedSchema _ =
    mempty
      |> example     ?~ "myawesomedomain.com"
      |> description ?~ "A domain name"
      |> type_       ?~ SwaggerString
      |> NamedSchema (Just "Subdomain")
      |> pure

instance ToParamSchema Subdomain where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance FromHttpApiData Subdomain where
  parseUrlPiece = Right . Subdomain

instance FromJSON Subdomain where
  parseJSON = withText "AWS.Subdomain" \txt ->
    Subdomain <$> parseJSON (String txt)

instance MimeRender PlainText Subdomain where
  mimeRender _ = UTF8.textToLazyBS . get

instance MimeRender OctetStream Subdomain where
  mimeRender _ = UTF8.textToLazyBS . get

instance MimeUnrender PlainText Subdomain where
  mimeUnrender _proxy bs =
    bs
      |> Lazy.toStrict
      |> decodeUtf8'
      |> bimap show Subdomain

instance Arbitrary Subdomain where
  arbitrary = do
    mayOpinion  <- frequency <| twoThirds opinions
    maySize     <- frequency <| twoThirds sizes
    mayAge      <- frequency <| twoThirds ages
    mayShape    <- frequency <| twoThirds shapes
    mayColour   <- frequency <| twoThirds colours
    mayMaterial <- frequency <| twoThirds materials

    let
      adjectives :: [Text]
      adjectives = catMaybes
        [ mayOpinion
        , maySize
        , mayAge
        , mayShape
        , mayColour
        , mayMaterial
        ]

    if null adjectives
      then
        arbitrary

      else do
        noun <- oneof <| pure <$> nouns
        return . Subdomain <| Text.intercalate "-" (adjectives <> [noun])

opinions :: [Text]
opinions =
  [ "really"
  , "amazing"
  , "beautiful"
  , "ugly"
  , "fugly"
  , "quick"
  , "wonderful"
  , "slim"
  , "awesome"
  , "sweet"
  , "tubular"
  , "eager"
  , "magnificient"
  , "nice"
  , "lively"
  , "bewildered"
  , "fierce"
  , "jolly"
  , "victorious"
  , "calm"
  , "brave"
  , "proud"
  , "fancy"
  , "skinny"
  , "bald"
  , "elegant"
  , "muscular"
  , "rich"
  , "tasty"
  , "super"
  , "brainy"
  , "infantile"
  , "juvenile"
  , "spry"
  , "playful"
  , "loyal"
  , "vicious"
  , "cute"
  , "benevolent"
  , "malevolent"
  , "universal"
  , "narcissistic"
  , "wise"
  , "stupid"
  , "obtuse"
  , "fun"
  , "charming"
  ]

sizes :: [Text]
sizes =
  [ "big"
  , "small"
  , "little"
  , "huge"
  , "enormous"
  , "petite"
  , "tall"
  , "short"
  , "tiny"
  , "colossal"
  , "long"
  , "short"
  , "long"
  , "gigantic"
  ]

ages :: [Text]
ages =
  [ "old"
  , "young"
  , "ancient"
  , "elderly"
  , "senior"
  , "junior"
  ]

shapes :: [Text]
shapes =
  [ "wide"
  , "narrow"
  , "round"
  , "triangular"
  , "square"
  , "flat"
  , "thin"
  , "thick"
  , "skinny"
  , "aerodynamic"
  , "oval"
  , "angular"
  ]

colours :: [Text]
colours =
  [ "red"
  , "green"
  , "blue"
  , "yellow"
  , "orange"
  , "teal"
  , "white"
  , "black"
  , "purple"
  , "pink"
  , "magenta"
  , "cyan"
  , "brown"
  , "maroon"
  , "aquamarine"
  , "fuchsia"
  , "crimson"
  , "scarlet"
  , "turquoise"
  ]

materials :: [Text]
materials =
  [ "glass"
  , "wooden"
  , "metalic"
  , "leather"
  , "polyester"
  , "silk"
  , "velvet"
  , "nylon"
  , "stone"
  , "diamond"
  , "plastic"
  , "copper"
  , "gold"
  , "silver"
  , "tin"
  , "platinum"
  , "carbon"
  , "cardboard"
  , "paper"
  , "sand"
  , "plaster"
  , "silicon"
  , "canvas"
  , "wool"
  , "cotton"
  , "marble"
  ]

nouns :: [Text]
nouns =
  [ "dragon"
  , "unicorn"
  , "mermaid"
  , "fairy"
  , "werewolf"
  , "sphinx"
  , "yeti"
  , "griffin"
  , "wolf"
  , "crow"
  , "centaur"
  , "imp"
  , "ghoul"
  , "pixie"
  , "gnome"
  , "wizard"
  , "witch"
  , "mage"
  , "troll"
  , "cat"
  , "dog"
  , "snake"
  , "lion"
  , "monkey"
  , "tiger"
  , "fish"
  , "crab"
  , "shark"
  , "salmon"
  , "tuna"
  , "horse"
  , "turtle"
  , "dolphin"
  , "deer"
  , "leopard"
  , "bear"
  , "frog"
  , "llama"
  , "penguin"
  , "pig"
  , "eagle"
  , "bat"
  , "vampire"
  , "dinosaur"
  , "whale"
  , "king"
  , "queen"
  , "jester"
  , "butterfly"
  , "tulip"
  , "polar-bear"
  , "cactus"
  , "hero"
  , "knight"
  , "hippogriff"
  , "elf"
  , "beast"
  , "sprite"
  , "alien"
  , "ghost"
  , "martian"
  , "princess"
  , "prince"
  , "chef"
  , "barista"
  ]

twoThirds :: [Text] -> [(Int, Gen (Maybe Text))]
twoThirds txts = (length txts * 3, pure Nothing) : (twoChances <$> txts)
  where
    twoChances :: Text -> (Int, Gen (Maybe Text))
    twoChances txt = (2, pure <| Just txt)
