{-# LANGUAGE DeriveAnyClass #-}

module Fission.Platform.Heroku.AddOn
  ( AddOn(..)
  -- Selectors
  , addOnID'
  , uuid'
  , region'
  , insertedAt'
  , modifiedAt'
  -- Lenses
  , addOnID
  , uuid
  , region
  , insertedAt
  , modifiedAt
  -- Table
  , tableName
  , addOns
  ) where

import RIO

import Control.Lens (makeLenses)
import Data.UUID
import Database.Selda

import           Fission.Internal.Orphanage ()
import           Fission.Platform.Heroku.Region (Region (..))
import           Fission.Storage.Mutate
import qualified Fission.Storage.Table       as Table

data AddOn = AddOn
  { _addOnID    :: ID AddOn
  , _uuid       :: UUID
  , _region     :: Maybe Region
  -- , refreshToken :: Text
  , _insertedAt :: UTCTime
  , _modifiedAt :: UTCTime
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )

makeLenses ''AddOn

instance DBInsertable AddOn where
  insertX t partRs = insertWithPK addOns $ fmap (insertStamp t) partRs

addOnID'    :: Selector AddOn (ID AddOn)
uuid'       :: Selector AddOn UUID
region'     :: Selector AddOn (Maybe Region)
insertedAt' :: Selector AddOn UTCTime
modifiedAt' :: Selector AddOn UTCTime

addOnID' :*: uuid'
         :*: region'
         :*: insertedAt'
         :*: modifiedAt' = selectors addOns

tableName :: Table.Name AddOn
tableName = "heroku_add_ons"

addOns :: Table AddOn
addOns = Table.lensPrefixed (Table.name tableName) [#_addOnID :- autoPrimary]
