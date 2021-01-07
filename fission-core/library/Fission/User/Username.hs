-- | Top-level username module

module Fission.User.Username
  ( toDNS
  --
  , module Fission.User.Username.Types
  , module Fission.User.Username.Error
  , module Fission.User.Username.Validation
  ) where

import qualified Network.DNS.Types                as DNS

import           Fission.Prelude

import           Fission.URL.Types

-- Reexports

import           Fission.User.Username.Error
import           Fission.User.Username.Types
import           Fission.User.Username.Validation

toDNS :: Username -> URL -> DNS.Domain
toDNS username serviceHost@URL {subdomain = serviceSubdomain} =
  encodeUtf8 txt
  where
    txt = textDisplay serviceHost {subdomain = Just didSegment <> serviceSubdomain}
    didSegment = Subdomain ("_did." <> textDisplay username)
