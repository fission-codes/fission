module Fission.URL
  ( prefix'
  , zeroOrOneSubdomain
  , toBaseUrl
  , fromBaseUrl
  , module Fission.URL.DomainName
  , module Fission.URL.Subdomain
  , module Fission.URL.Types
  ) where

import qualified RIO.Text                       as Text

import           Servant.Client.Core.BaseUrl

import           Fission.Prelude

import           Fission.Error.InvalidURL.Types

-- Reexport

import           Fission.URL.DomainName
import           Fission.URL.Subdomain
import           Fission.URL.Types

prefix' :: Subdomain -> URL -> URL
prefix' moreSub url@URL {..} = url { subdomain = Just moreSub <> subdomain }

zeroOrOneSubdomain :: URL -> Either InvalidURL URL
zeroOrOneSubdomain url =
  case take 3 . Text.split (== '.') $ textDisplay url of
    [sub, domain, tld] -> Right $ URL (DomainName $ domain <> "." <> tld) (Just $ Subdomain sub)
    [domain, tld]      -> Right $ URL (DomainName $ domain <> "." <> tld) Nothing
    _ -> Left InvalidURL

fromBaseUrl :: BaseUrl -> URL
fromBaseUrl BaseUrl {..} =
    URL { domainName = DomainName $ Text.pack baseUrlHost
        , subdomain  = Nothing
        }

toBaseUrl :: Scheme -> URL -> Int -> BaseUrl
toBaseUrl scheme url port =
  BaseUrl { baseUrlScheme = scheme
          , baseUrlHost   = show url
          , baseUrlPort   = port
          , baseUrlPath   = ""
          }
