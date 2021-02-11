-- | The name of a Fission app (i.e. the subdomain in Fission's app namespace)
module Fission.App.Name.Types
  ( Name
  , raw
  , mkName
  ) where

import qualified RIO.ByteString.Lazy    as Lazy
import qualified RIO.Text               as Text

import           Data.Swagger           hiding (name)
import           Servant.API

import           Fission.Prelude

import qualified Fission.App.Name.Error as App.Name
import           Fission.URL.Validation

newtype Name = Name { raw :: Text }
  deriving newtype (Eq, Show, Display)

mkName :: Text -> Either App.Name.Invalid Name
mkName txt =
  if isValid normalized
    then Right $ Name normalized
    else Left App.Name.Invalid

  where
    normalized = Text.toLower txt

instance FromJSON Name where
  parseJSON = withText "App.Name" \txt ->
    case mkName txt of
      Left  _    -> fail $ Text.unpack (txt <> "is an invalid app name")
      Right name -> return name

instance ToJSON Name where
  toJSON (Name name) = String name

instance ToParamSchema Name where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance FromHttpApiData Name where
  parseUrlPiece txt =
    case mkName txt of
      Left _err  -> Left $ "Invalid app name: " <> txt
      Right name -> Right name

instance ToHttpApiData Name where
  toUrlPiece = textDisplay

instance MimeRender PlainText Name where
  mimeRender _ = displayLazyBS . raw

instance MimeRender OctetStream Name where
  mimeRender _ = displayLazyBS . raw

instance MimeUnrender PlainText Name where
  mimeUnrender _proxy bs =
    case decodeUtf8' $ Lazy.toStrict bs of
      Left unicodeErr ->
        Left $ mconcat
          [ "App name "
          , show bs
          , " contains invalid non-unicode character(s): "
          , show unicodeErr
          ]

      Right txt ->
        case mkName txt  of
          Left  _err -> Left . show $ "Invalid app name: " <> bs
          Right name -> Right name

instance Arbitrary Name where
  arbitrary = do
    generators <- sublistOf [opinions, sizes, ages, shapes, colours, materials]

    if null generators
      then
        arbitrary

      else do
        adjectives <- sequence (elements <$> take 3 generators)
        noun       <- elements nouns
        return . Name . Text.intercalate "-" $ adjectives <> [noun]

opinions :: [Text]
opinions =
  [ "amazing"
  , "awesome"
  , "bald"
  , "beautiful"
  , "benevolent"
  , "bewildered"
  , "brainy"
  , "brave"
  , "calm"
  , "charming"
  , "cool"
  , "cute"
  , "eager"
  , "elegant"
  , "fancy"
  , "fierce"
  , "fun"
  , "gnarly"
  , "infantile"
  , "jolly"
  , "juvenile"
  , "lively"
  , "loyal"
  , "magnificient"
  , "malevolent"
  , "muscular"
  , "narcissistic"
  , "nice"
  , "obtuse"
  , "playful"
  , "proud"
  , "quick"
  , "rich"
  , "skinny"
  , "spry"
  , "stupid"
  , "super"
  , "sweet"
  , "tasty"
  , "tubular"
  , "ugly"
  , "universal"
  , "venerable"
  , "vicious"
  , "victorious"
  , "wicked"
  , "wise"
  , "wonderful"
  ]

sizes :: [Text]
sizes =
  [ "big"
  , "colossal"
  , "enormous"
  , "gigantic"
  , "huge"
  , "little"
  , "long"
  , "long"
  , "petite"
  , "short"
  , "short"
  , "small"
  , "tall"
  , "tiny"
  ]

ages :: [Text]
ages =
  [ "old"
  , "young"
  , "ancient"
  , "elderly"
  , "geriatric"
  , "senior"
  , "junior"
  ]

shapes :: [Text]
shapes =
  [ "aerodynamic"
  , "angular"
  , "flat"
  , "narrow"
  , "oval"
  , "round"
  , "skinny"
  , "square"
  , "thick"
  , "thin"
  , "triangular"
  , "wide"
  ]

colours :: [Text]
colours =
  [ "aquamarine"
  , "azure"
  , "black"
  , "blue"
  , "brown"
  , "crimson"
  , "cyan"
  , "fuchsia"
  , "green"
  , "magenta"
  , "maroon"
  , "orange"
  , "pink"
  , "purple"
  , "red"
  , "scarlet"
  , "teal"
  , "turquoise"
  , "violet"
  , "white"
  , "yellow"
  ]

materials :: [Text]
materials =
  [ "brass"
  , "canvas"
  , "carbon"
  , "cardboard"
  , "cotton"
  , "diamond"
  , "fur"
  , "glass"
  , "iron"
  , "leather"
  , "marble"
  , "metalic"
  , "nylon"
  , "paper"
  , "plaster"
  , "plastic"
  , "polyester"
  , "sand"
  , "silicon"
  , "silk"
  , "steel"
  , "stone"
  , "tin"
  , "velvet"
  , "wooden"
  , "wool"
  ]

nouns :: [Text]
nouns =
  [ "alien"
  , "artist"
  , "barista"
  , "bat"
  , "bear"
  , "beast"
  , "bigfoot"
  , "butterfly"
  , "cactus"
  , "cat"
  , "centaur"
  , "chef"
  , "crab"
  , "crow"
  , "cyclops"
  , "deer"
  , "dinosaur"
  , "dog"
  , "dolphin"
  , "dragon"
  , "eagle"
  , "elf"
  , "fairy"
  , "fish"
  , "frog"
  , "ghost"
  , "ghoul"
  , "gnome"
  , "goblin"
  , "golem"
  , "griffin"
  , "hero"
  , "hippogriff"
  , "horse"
  , "hydra"
  , "imp"
  , "jester"
  , "king"
  , "kitten"
  , "knight"
  , "leopard"
  , "lion"
  , "llama"
  , "mage"
  , "martian"
  , "mermaid"
  , "monkey"
  , "ogre"
  , "penguin"
  , "phoenix"
  , "pig"
  , "piglet"
  , "pixie"
  , "pony"
  , "prince"
  , "princess"
  , "puppy"
  , "queen"
  , "raptor"
  , "rocketship"
  , "salmon"
  , "shark"
  , "snake"
  , "sphinx"
  , "sprite"
  , "star"
  , "tiger"
  , "troll"
  , "tulip"
  , "tuna"
  , "turtle"
  , "ufo"
  , "unicorn"
  , "vampire"
  , "werewolf"
  , "whale"
  , "witch"
  , "wizard"
  , "wolf"
  , "wraith"
  , "yeti"
  , "zombie"
  ]
