module Fission.User.DID.Types
  ( PublicKey (..)
  , Algorithm (..)
  ) where

import qualified RIO.Text as Text

import           Data.Swagger
import           Database.Persist.Postgresql

import           Fission.Prelude

data Algorithm
  = EdDSA
  | ECDSA
  | SECP
  | RSA_4096
  deriving ( Eq
           , Generic
           , Show
           , ToJSON
           , FromJSON
           , ToSchema
           )

newtype PublicKey = PublicKey { unPublicKey :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToJSON
                    , FromJSON
                    , ToSchema
                    )

instance PersistField PublicKey where
  toPersistValue (PublicKey pk) = PersistText pk
  fromPersistValue = \case
    PersistText pk -> Right (PublicKey pk)
    other          -> Left ("Invalid Persistent PublicKey: " <> Text.pack (show other))

instance PersistFieldSql PublicKey where
  sqlType _pxy = SqlString
