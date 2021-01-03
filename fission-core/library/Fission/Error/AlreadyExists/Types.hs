{-# LANGUAGE UndecidableInstances #-}

module Fission.Error.AlreadyExists.Types (AlreadyExists (..)) where

import           Fission.Prelude

data AlreadyExists entity
  = AlreadyExists
  deriving ( Show
           , Eq
           , Exception
           )

-- instance Display (AlreadyExists Ed25519.SecretKey) where
  -- display _ = "Ed25519 secret key already exists"
