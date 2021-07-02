module Fission.Web.Server.Internal.NGINX.Purge.Error
  ( Error (..)
  , BatchErrors (..)
  ) where

import qualified RIO.NonEmpty                                      as NonEmpty
import           Servant.Server

import           Fission.Prelude                                   hiding
                                                                   (Error)

import           Fission.Web.Server.Internal.Orphanage.ServerError ()

---------------------------------------------------------------------------------

newtype Error = Error { serverError :: ServerError }
  deriving stock (Show, Eq)
  deriving anyclass Exception

instance Display Error where
  display (Error err) = "NGINX.Error=" <> display err

---------------------------------------------------------------------------------

newtype BatchErrors = BatchErrors { serverErrors :: NonEmpty ServerError }
  deriving stock (Show, Eq)
  deriving anyclass Exception

instance Display BatchErrors where
  display (BatchErrors errs) = "NGINX.BatchErrors=" <> display (NonEmpty.toList errs)
