module Fission.URL.Types
  ( URL (..)
  , module Fission.URL.DomainName.Types
  , module Fission.URL.Subdomain.Types
  ) where

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
