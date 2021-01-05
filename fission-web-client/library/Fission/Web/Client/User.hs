{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Fission.Web.Client.User
  ( createWithDID
  , createWithPassword
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

(createWithDID :<|> createWithPassword)
  :<|> whoami
  :<|> verify
  :<|> email
  :<|> did
  :<|> exchangeKeys
  :<|> dataRoot
  :<|> passwordReset = client $ Proxy @User
