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
      Left _err  -> Left $ "Invalid usrename: " <> txt
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
  , "beautiful"
  , "ugly"
  , "quick"
  , "wonderful"
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
  , "wicked"
  , "gnarly"
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
  , "tin"
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
  , "ufo"
  ]
