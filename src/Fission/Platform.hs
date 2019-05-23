{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Platform (Platform (..)) where

import Database.Selda
import RIO

data Platform
  = Direct
  | Heroku
  deriving ( Show
           , Read
           , Eq
           , Bounded
           , Enum
           , SqlType
           )
