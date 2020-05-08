module Fission.URL
  ( prefix'
  , module Fission.URL.DomainName
  , module Fission.URL.Subdomain
  , module Fission.URL.Types
  ) where

import Fission.Prelude

-- Reexport

import Fission.URL.Types
import Fission.URL.DomainName
import Fission.URL.Subdomain

prefix' :: Subdomain -> URL -> URL
prefix' moreSub url@URL {..} = url { subdomain = Just moreSub <> subdomain }
