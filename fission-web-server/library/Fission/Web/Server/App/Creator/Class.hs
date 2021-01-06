module Fission.Web.Server.App.Creator.Class
  ( Creator (..)
  , Errors'
  ) where

import           Servant

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Get.Error                             as IPFS.Stat

import           Fission.Prelude

import           Fission.Error                                      as Error
import           Fission.URL

import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import           Fission.Web.Server.Models

import qualified Fission.Web.Server.App.Domain                      as App.Domain

type Errors' = OpenUnion
  '[ ServerError
   , App.Domain.AlreadyAssociated

   , ActionNotAuthorized App
   , NotFound            App

   , ActionNotAuthorized URL
   , NotFound            URL

   , IPFS.Stat.Error

   , InvalidURL
   ]

class Monad m => Creator m where
  create ::
       UserId
    -> CID
    -> Maybe Subdomain
    -> UTCTime
    -> m (Either Errors' (AppId, Subdomain))
