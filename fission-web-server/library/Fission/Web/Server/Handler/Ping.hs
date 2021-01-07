module Fission.Web.Server.Handler.Ping (handler) where

import           Fission.Prelude

import           Fission.Pong.Types

handler :: Monad m => m Pong
handler = return $ Pong "pong"
