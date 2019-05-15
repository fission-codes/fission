{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Fission.User where

import RIO

import Control.Lens (makeLenses)

import Database.Beam
import Database.Beam.Sqlite

data UserT f = User
  { _userEmail    :: Columnar f Text
  , _userPassword :: Columnar f Text
  }
  deriving (Generic, Beamable)

makeLenses ''UserT

type User   =            UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f Text)
    deriving (Generic, Beamable)

  primaryKey = UserId . _userEmail
