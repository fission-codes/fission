module Fission.Web.Auth.Token.JWT.Proof.WNFS
  ( pathSubset
  , isPrivate
  , bitwiseContains
  , normalizePath
  ) where

import qualified RIO.ByteString                                   as BS
import qualified RIO.List                                         as List
import qualified RIO.Text                                         as Text

import qualified Data.Bits                                        as Bits

import           Fission.Prelude

import Web.JWT.Proof.Class
import           Web.JWT.Proof.Error
import           Web.JWT.Types                                    as JWT

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types
