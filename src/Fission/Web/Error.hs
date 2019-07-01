module Fission.Web.Error (ensureUnicode) where

import RIO

import Servant

import qualified Fission.Internal.UTF8 as UTF8

ensureUnicode :: (MonadThrow m, Show a) => Either a b -> m b
ensureUnicode (Right ok) = pure ok
ensureUnicode (Left err) = throwM $ err500 { errBody = UTF8.showLazyBS err }
