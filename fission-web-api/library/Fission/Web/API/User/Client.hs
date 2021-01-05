{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Fission.Web.API.User.Client
  ( create
  , whoami
  , verify
  , email
  , did
  , exchangeKeys
  , dataRoot
  , passwordReset
  ) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.User.Types

create
 :<|> whoami
 :<|> verify
 :<|> email
 :<|> did
 :<|> exchangeKeys
 :<|> dataRoot
 :<|> passwordReset = client $ Proxy @User
