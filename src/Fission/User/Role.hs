{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.User.Role
  ( Role (..)
  , _Regular
  , _Admin
  ) where

import RIO hiding (id)

import Control.Lens   (makePrisms)
import Database.Selda (SqlType)

data Role
  = Regular
  | Admin
  deriving ( Show
           , Read
           , Eq
           , Enum
           , Bounded
           , SqlType
           )

makePrisms ''Role
