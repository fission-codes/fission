module Fission.URL.Types
  ( URL (..)
  , module Fission.URL.DomainName.Types
  , module Fission.URL.Subdomain.Types
  ) where

import           Data.Swagger hiding (URL)
import           Servant

import qualified RIO.List as List
import qualified RIO.Text as Text

import           Fission.Prelude

import           Fission.URL.DomainName.Types
import           Fission.URL.Subdomain.Types

data URL = URL
  { domainName :: DomainName
  , subdomain  :: Maybe Subdomain
  }
  deriving ( Show
           , Eq
           )

instance FromHttpApiData URL where
  parseUrlPiece txt =
    if any (== "") tokens
      then err
      else parse Nothing tokens

    where
      tokens :: [Text]
      tokens = Text.split (== '.') txt

      parse :: Maybe Text -> [Text] -> Either Text URL
      parse _       []           = err
      parse _       [_]          = err
      parse acc     [dom, tld]   = Right <| URL (DomainName (dom <> "." <> tld)) (Subdomain <$> acc)
      parse Nothing (sub : more) = parse (Just sub)        more
      parse acc     (sub : more) = parse (acc <> Just ("." <> sub)) more

      err :: Either Text URL
      err = Left "Improperly formatted URL"

instance ToParamSchema URL where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance FromJSON URL where
  parseJSON = withText "URL" \txt -> do
    let
      sections                    = Text.split (== '.') txt
      (subdomainSecs, domainSecs) = List.splitAt (length sections - 2) sections

      maySubdomain = case subdomainSecs of
        [] -> Nothing
        _  -> Just . Subdomain <| Text.intercalate "." subdomainSecs

    case domainSecs of
      [domain, tld] ->
        return URL
          { domainName = DomainName (domain <> "." <> tld)
          , subdomain  = maySubdomain
          }

      _ ->
        fail "Not a valid domain name"
