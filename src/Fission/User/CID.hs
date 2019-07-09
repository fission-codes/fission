-- | Users of 'Fission', accounts, properties, &c
module Fission.User.CID
  ( UserCID (..)
  -- Selectors
  , userCID'
  , userFK'
  , cid'
  , insertedAt'
  , modifiedAt'
  -- Lenses
  , userCID
  , userFK
  , cid
  , insertedAt
  , modifiedAt
  -- Table
  , userCIDs
  , tableName
  -- Helpers
  , createFresh
  ) where

import           RIO

import Control.Lens (makeLenses, (.~))
import Database.Selda
import Data.Time    (getCurrentTime)
import Data.Swagger

import           Fission.IPFS.CID.Types
import           Fission.User (User (..))

import           Fission.Storage.Mutate
import qualified Fission.Storage.Table  as Table

import Fission.Internal.Constraint
import Fission.Internal.Orphanage ()

-- | A user account, most likely a developer
data UserCID = UserCID
  { _userCID       :: ID UserCID
  , _userFK        :: ID User
  , _cid           :: Text -- SqlType for CID was getting hairy
  , _insertedAt    :: UTCTime
  , _modifiedAt    :: UTCTime
  } deriving ( Show
             , Eq
             , Generic
             , SqlRow
             )

makeLenses ''UserCID

instance DBInsertable UserCID where
  insertX t partRs = insertWithPK userCIDs $ fmap (insertStamp t) partRs

instance ToSchema (ID UserCID) where
  declareNamedSchema _ =
     return $ NamedSchema (Just "UserCID")
            $ mempty & type_ .~ SwaggerInteger

userCID'    :: Selector UserCID (ID UserCID)
userFK'     :: Selector UserCID (ID User)
cid'        :: Selector UserCID Text
insertedAt' :: Selector UserCID UTCTime
modifiedAt' :: Selector UserCID UTCTime

userCID' :*: userFK'
         :*: cid'
         :*: insertedAt'
         :*: modifiedAt' = selectors userCIDs

-- | The name of the 'users' table
tableName :: Table.Name UserCID
tableName = "user_cids"

-- | The 'User' table
userCIDs :: Table UserCID
userCIDs = Table.lensPrefixed (Table.name tableName)
  [ #_userCID :- autoPrimary
  , #_userFK  :- index
  , #_cid     :- index
  ]

-- | Create a new, timestamped entry
createFresh :: MonadRIO   cfg m
            => HasLogFunc cfg
            => MonadSelda     m
            => ID User
            -> CID
            -> m (ID UserCID)
createFresh userID (CID cidTxt) = transaction do
  now <- liftIO getCurrentTime
  ucid <- insert1 now $ UserCID def userID cidTxt
  logInfo $ "Inserted user CID " <> display ucid
  return ucid
