{-# LANGUAGE TypeFamilies #-}
module Fission.Config.Foo where

import RIO

import Database.Beam
import Database.Beam.Sqlite

import Data.Text              (Text)
import Database.SQLite.Simple

data UserT f
    = User
    { _userEmail     :: Columnar f Text
    , _userFirstName :: Columnar f Text
    , _userLastName  :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = UserId . _userEmail

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                        deriving (Generic, Database be)
data AddressT f = Address
                { _addressId      :: C f Int
                , _addressLine1   :: C f Text
                , _addressLine2   :: C f (Maybe Text)
                , _addressCity    :: C f Text
                , _addressState   :: C f Text
                , _addressZip     :: C f Text

                , _addressForUser :: PrimaryKey UserT f }
                  deriving (Generic, Beamable)
type Address = AddressT Identity
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = AddressId . _addressId

type AddressId = PrimaryKey AddressT Identity
shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

foo :: IO ()
foo = do
  conn <- open "shoppingcart1.db"
  runBeamSqliteDebug (const $ pure ()) {- for debug output -} conn $ runInsert $
    insert (_shoppingCartUsers shoppingCartDb) $
    insertValues [ User "jAmes@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
