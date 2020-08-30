module Fission.Challenge.Types (Challenge(..)) where
  
import           Database.Persist.Class
import           Database.Persist.Types
import           Database.Persist.Sql

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import qualified Network.IPFS.Internal.UTF8 as UTF8

import           Data.Swagger
import           Servant

import           Fission.Prelude


newtype Challenge = Challenge { unChallenge :: Text }
  deriving newtype  ( Eq
                    , Show
                    , IsString
                    , ToHttpApiData
                    )

instance Arbitrary Challenge where
  arbitrary = Challenge <$> arbitrary

instance ToJSON Challenge where
  toJSON (Challenge challenge') = String challenge'

instance FromJSON Challenge where
  parseJSON = withText "Challenge" (pure . Challenge)

instance FromHttpApiData Challenge where
  parseUrlPiece = Right . Challenge

instance PersistField Challenge where
  toPersistValue (Challenge challenge') = PersistText challenge'
  fromPersistValue = \case
    PersistText challenge' -> Right (Challenge challenge')
    other                  -> Left ("Invalid Persistent Challenge: " <> Text.pack (show other))

instance PersistFieldSql Challenge where
  sqlType _pxy = SqlString

instance MimeRender PlainText Challenge where
  mimeRender _ = UTF8.textToLazyBS . unChallenge

instance MimeUnrender PlainText Challenge where
  mimeUnrender _ = bimap show Challenge . decodeUtf8' . Lazy.toStrict

instance ToParamSchema Challenge where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance ToSchema Challenge where
  declareNamedSchema _ =
    mempty
      |> type_       ?~ SwaggerString
      |> description ?~ "A hash representing a user challenge. Sent in a verification email"
      |> example     ?~ toJSON (Challenge "KyGig3J8lx3K07gZMK1lJV9M6Pr8w2RB")
      |> NamedSchema (Just "Challenge")
      |> pure

