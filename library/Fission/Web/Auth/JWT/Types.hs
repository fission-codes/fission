module Fission.Web.Auth.JWT.Types
  ( Token     (..)
  , Header    (..)
  , Payload   (..)

  -- * reexports
  , module Fission.Web.Auth.JWT.Algorithm.Types
  , module Fission.Web.Auth.JWT.Typ.Types
  , module Fission.Web.Auth.JWT.Payload.Types
  ) where


import qualified Crypto.PubKey.Ed25519 as Ed

import           Fission.Prelude
import           Fission.User.DID.Types
 
import           Fission.Web.Auth.JWT.Algorithm.Types
import           Fission.Web.Auth.JWT.Typ.Types
import           Fission.Web.Auth.JWT.Payload.Types

data Token = Token
  { header  :: !Header
  , payload :: !Payload
  , sig     :: !Ed.Signature
  } deriving (Show, Generic)

data Header = Header
  { typ :: !Typ
  , alg :: !Algorithm
  } deriving ( Show
             , Generic
             , FromJSON
             , ToJSON
             )
