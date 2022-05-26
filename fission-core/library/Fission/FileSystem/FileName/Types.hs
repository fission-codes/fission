module Fission.FileSystem.FileName.Types (FileName (..)) where

import           RIO.Text                   as Text

import           Data.Swagger
import           Servant.API

import           Fission.Prelude

import           Fission.URL.Validation

import           Fission.FileSystem.FileName.Error


newtype FileName = FileName { fileName :: Text }
  deriving          ( Generic )
  deriving anyclass ( ToSchema
                    , ToParamSchema
                    )
  deriving newtype  ( Eq
                    , Show
                    , IsString
                    )

mkDirectoryName :: Text -> Either Invalid FileName
mkDirectoryName txt =
  if isValid normalized
    then Right $ FileName normalized
    else Left Invalid

  where
    normalized = Text.toLower txt

instance Arbitrary FileName where
  arbitrary = do
    txt <- arbitrary
    case mkDirectoryName $ Text.filter isURLChar txt of
      Left _      -> arbitrary
      Right dName -> return dName

instance FromHttpApiData FileName where
  parseUrlPiece = Right . FileName

instance ToHttpApiData FileName where
  toUrlPiece (FileName fileName) = fileName