module Fission.Web.Auth.JWT.Types
  ( Token     (..)
  , Header    (..)
  , Payload   (..)
  , Algorithm (..)
  , Typ       (..)
  ) where

import           Fission.Prelude

import           Fission.User.DID.Types
import qualified Crypto.PubKey.Ed25519 as Ed

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

data Payload = Payload
  { iss        :: !DID
  , nbf        :: !Int
  , exp        :: !Int
  } deriving ( Show
             , Generic
             , FromJSON
             , ToJSON
             )

newtype Typ = Typ { unTyp :: ByteString }
  -- deriving anyclass (Generic)
  deriving newtype  ( Show
                    , Eq
                    , IsString
                    )

instance FromJSON Typ where
  parseJSON = withText "JWT.Typ" (pure . Typ . encodeUtf8)

instance ToJSON Typ where
  toJSON (Typ bs) = String <| decodeUtf8Lenient bs

newtype Algorithm = Algorithm { unAlgorithm :: ByteString }
  deriving newtype ( IsString
                   , Show
                   , Eq
                   )

instance FromJSON Algorithm where
  parseJSON = withText "JWT.Algorithm" (pure . Algorithm . encodeUtf8)

instance ToJSON Algorithm where
  toJSON (Algorithm bs) = String <| decodeUtf8Lenient bs
