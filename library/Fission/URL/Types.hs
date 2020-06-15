module Fission.URL.Types
  ( URL (..)
  , module Fission.URL.DomainName.Types
  , module Fission.URL.Subdomain.Types
  , module Fission.URL.Path.Types
  ) where

import           Data.Swagger hiding (URL)
import           Servant

import qualified RIO.List as List
import qualified RIO.Text as Text

import           Fission.Prelude

import           Fission.URL.DomainName.Types
import           Fission.URL.Subdomain.Types
import           Fission.URL.Path.Types

data URL = URL
  { domainName :: DomainName
  , subdomain  :: Maybe Subdomain
  }

instance Eq URL where
  urlA == urlB = textDisplay urlA == textDisplay urlB

instance Arbitrary URL where
  arbitrary = URL <$> arbitrary <*> arbitrary

instance Display URL where
  display (URL domain Nothing)    = display domain
  display (URL domain (Just sub)) = display sub <> "." <> display domain

instance Show URL where
  show = Text.unpack . textDisplay

instance ToHttpApiData URL where
  toUrlPiece = textDisplay

instance FromHttpApiData URL where
  parseUrlPiece txt =
    if any Text.null tokens
      then err
      else parse Nothing tokens

    where
      tokens :: [Text]
      tokens = Text.split (== '.') txt

      parse :: Maybe Text -> [Text] -> Either Text URL
      parse _       []           = err
      parse _       [_]          = err
      parse acc     [dom, tld]   = Right $ URL (DomainName (dom <> "." <> tld)) (Subdomain <$> acc)
      parse Nothing (sub : more) = parse (Just sub)        more
      parse acc     (sub : more) = parse (acc <> Just ("." <> sub)) more

      err :: Either Text URL
      err = Left "Improperly formatted URL"

instance ToParamSchema URL where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance ToSchema URL where
  declareNamedSchema _ =
    mempty
      |> type_       ?~ SwaggerString
      |> description ?~ "A domain name, potentially with a subdomain"
      |> example     ?~ toJSON (URL (DomainName "example.com") (Just (Subdomain "foo")))
      |> NamedSchema (Just "URL")
      |> pure

instance ToJSON URL where
  toJSON URL {..} = String $ normalizedSubdomain <> domain
    where
      DomainName domain = domainName

      normalizedSubdomain = case subdomain of
        Nothing -> ""
        Just (Subdomain sub) -> sub <> "."

instance FromJSON URL where
  parseJSON = withText "URL" \txt -> do
    let
      noProtocol = Text.dropPrefix "https://" $ Text.dropPrefix "http://" txt
      sections   = Text.split (== '.') noProtocol
      (subdomainSecs, domainSecs) = List.splitAt (length sections - 2) sections

      maySubdomain = case subdomainSecs of
        [] -> Nothing
        _  -> Just . Subdomain $ Text.intercalate "." subdomainSecs

    case domainSecs of
      [domain, tld] ->
        return URL
          { domainName = DomainName (domain <> "." <> tld)
          , subdomain  = maySubdomain
          }

      _ ->
        fail "Not a valid domain name"
