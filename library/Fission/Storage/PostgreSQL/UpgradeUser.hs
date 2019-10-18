-- | Table creation and migration sequences
module Fission.Storage.PostgreSQL.UpgradeUser
  ( upgrade
  ) where

import RIO

import Database.Selda.PostgreSQL
import Database.Selda.Migrations
import Database.Selda.Backend.Internal
import Database.Selda.MakeSelectors

import Database.Selda

import qualified Fission.Storage.Table  as Table
import qualified Fission.User.Table  as User.Table
import           Fission.User.Types

import Control.Lens   ((?~))
import Data.Swagger
import Database.Selda

import Fission.Internal.Constraint
import Fission.Storage.PostgreSQL

import qualified Fission.Platform.Heroku.AddOn as Heroku

import Fission.Security       (Digestable (..))
import Fission.Security.Types (SecretDigest)
import Fission.User.Role
import Fission.User.Selector
import Fission.User.Security
import qualified Fission.Internal.UTF8 as UTF8


-- upgrade :: MonadRIO   cfg m
--         => MonadSelda     m
--         => MonadMask      m
--         => Relational     User
--         => Relational     OldUser
--         => MonadMask      m
--         => HasLogFunc cfg
--         => m ()
upgrade = migrate oldUsers User.Table.users \row -> new
  [ userID'        := toId . fromId <$> row ! userID''
  , username'      := text "username" -- TODO: hashMe <$> row ! userID''
  , email'         := null_
  , role'          := row ! role''
  , active'        := row ! active''
  , herokuAddOnID' := row ! herokuAddOnID''
  , secretDigest'  := row ! secretDigest'' -- password field already?
  , insertedAt'    := row ! insertedAt''
  , modifiedAt'    := row ! modifiedAt''
  ]
          -- newID
          --             :*: text "username"
          --             :*: (null_ :: Col User (Maybe Text))
          --             :*: _role
          --             :*: _active
          --             :*: _herokuAddOnID
          --             :*: _secretDigest
          --             :*: _insertedAt
          --             :*: _modifiedAt
        -- res
            -- return User { _userID
            --               , _username = hashID _userID
            --               , _email = Nothing
            --               , _role
            --               , _active
            --               , _herokuAddOnID
            --               , _secretDigest
            --               , _insertedAt
            --               , _modifiedAt
            --           }

            -- return newUser
        -- undefined


-- | The 'User' table
oldUsers :: Table OldUser
oldUsers = Table.lensPrefixed (Table.name User.Table.name)
  [ #_userID        :- autoPrimary
  -- , #_username      :- index
  -- , #_username      :- unique
  , #_active        :- index
  , #_secretDigest  :- index
  , #_secretDigest  :- unique
  , #_herokuAddOnID :- foreignKey Heroku.addOns Heroku.addOnID'
  ]

-- | A user account, most likely a developer
data OldUser = OldUser
  { _userID        :: ID OldUser
  , _role          :: Role
  , _active        :: Bool
  , _herokuAddOnID :: Maybe (ID Heroku.AddOn)
  , _secretDigest  :: SecretDigest
  , _insertedAt    :: UTCTime
  , _modifiedAt    :: UTCTime
  } deriving ( Show
             , Eq
             , Generic
             , SqlRow
             )

instance Digestable (ID OldUser) where
  digest = digest . UTF8.textShow

instance ToSchema (ID OldUser) where
  declareNamedSchema _ =
     return $ NamedSchema (Just "UserID")
            $ mempty & type_ ?~ SwaggerInteger

userID'' :*: role''
         :*: active''
         :*: herokuAddOnID''
         :*: secretDigest''
         :*: insertedAt''
         :*: modifiedAt'' = selectors oldUsers
