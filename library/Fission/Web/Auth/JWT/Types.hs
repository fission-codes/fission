module Fission.Web.Auth.JWT.Types
  ( Token (..)
  , Header (..)
  , Payload (..)
  ) where

import Fission.Prelude

import           Fission.User.DID.Types
import qualified Crypto.PubKey.Ed25519 as Ed
import           Fission.Internal.Orphanage.ByteString ()

data Token = Token
  { header  :: !Header
  , payload :: !Payload
  , sig     :: !Ed.Signature
  } deriving (Show, Generic)

data Header = Header
  { typ :: !ByteString
  , alg :: !ByteString
  } deriving ( Show
             , Generic
             , FromJSON
             , ToJSON
             )

data Payload = Payload
  { iss        :: !DID
  , nbf        :: !Integer
  , exp        :: !Integer
  } deriving ( Show
             , Generic
             , FromJSON
             , ToJSON
             )
