module Fission.URL
  ( prefix'
  , zeroOrOneSubdomain
  , module Fission.URL.DomainName
  , module Fission.URL.Subdomain
  , module Fission.URL.Types
  , module Fission.URL.Errors
  ) where

import qualified RIO.Text as Text

import           Fission.Prelude
 
-- Reexport

import           Fission.URL.Errors
import           Fission.URL.Types
import           Fission.URL.DomainName
import           Fission.URL.Subdomain

prefix' :: Subdomain -> URL -> URL
prefix' moreSub url@URL {..} = url { subdomain = Just moreSub <> subdomain }

zeroOrOneSubdomain :: URL -> Either InvalidURL URL
zeroOrOneSubdomain url =
  case take 3 . Text.split (== '.') $ textDisplay url of
    [sub, domain, tld] -> Right $ URL (DomainName $ domain <> "." <> tld) (Just $ Subdomain sub)
    [domain, tld]      -> Right $ URL (DomainName $ domain <> "." <> tld) Nothing
    _ -> Left InvalidURL
