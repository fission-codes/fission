{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.User where

import RIO

import Control.Lens (makeLenses)

data User = User
  { _email    :: Text
  , _password :: Text
  }

makeLenses ''User
