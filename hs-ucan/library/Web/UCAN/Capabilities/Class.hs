module Web.UCAN.Capabilities.Class
  ( IsResource(..)
  , IsAbility(..)
  , parseResource
  , renderResource
  , parseAbility
  , renderAbility
  ) where

import           Data.Aeson.Types
import           RIO
import qualified RIO.Text         as Text
import qualified Text.Megaparsec  as Megaparsec
import           Text.URI         (URI)
import qualified Text.URI         as URI


class IsResource res where
  parseResourceURI :: URI -> Maybe res
  renderResourceURI :: res -> URI

  parseResourceV_0_3 :: Text -> Maybe res
  parseResourceV_0_3 _ = Nothing


parseResource :: IsResource res => Text -> Result res
parseResource text = case parseResourceURI =<< Megaparsec.parseMaybe (URI.parser @Void) text of
  Just uri -> Success uri
  Nothing  -> Error $ "Couldn't parse capability resource URI: " <> Text.unpack text

renderResource :: IsResource res => res -> Text
renderResource resource = URI.render $ renderResourceURI resource


class IsAbility abl where
  parseAbilityParts :: (Text, Text) -> Maybe abl
  renderAbilityParts :: abl -> (Text, Text)

  parseAbilityV_0_3 :: Text -> Maybe abl
  parseAbilityV_0_3 _ = Nothing

parseAbility :: IsAbility abl => Text -> Result abl
parseAbility text = case Text.split (== '/') $ Text.toLower text of
  (head:rest) -> case parseAbilityParts (head, Text.intercalate "/" rest) of
    Just ability -> Success ability
    Nothing      -> Error $ "Cannot parse ability: " <> show text
  []          -> Error $ "Cannot parse ability: " <> show text


renderAbility :: IsAbility abl => abl -> Text
renderAbility ability =
    let (head, rest) = renderAbilityParts ability
    in head <> "/" <> rest
