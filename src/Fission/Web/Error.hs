module Fission.Web.Error (ensureUnicode, throw) where

import RIO

import Servant

import qualified Fission.Internal.UTF8 as UTF8

import Data.Aeson
import Network.HTTP.Types.Status
import Servant.Exception

import Fission.Internal.Constraint

ensureUnicode :: (MonadThrow m, Show a) => Either a b -> m b
ensureUnicode (Right ok) = pure ok
ensureUnicode (Left err) = throwM $ err500 { errBody = UTF8.showLazyBS err }

throw :: MonadRIO   cfg m
      => HasLogFunc cfg
      => MonadThrow     m
      => Display      err
      => Exception    err
      => ToJSON       err
      => ToServantErr err
      => err
      -> m a
throw err = do
  when (statusIsServerError $ status err) (logError $ display err)
  throwM $ toServantException err
