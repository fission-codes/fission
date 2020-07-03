module Fission.App.Creator.Class
  ( Creator (..)
  , Errors
  ) where

import           Servant

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Get.Error as IPFS.Stat

import           Fission.Prelude
import           Fission.Error as Error
import           Fission.Models
import           Fission.URL

import qualified Fission.App.Domain as App.Domain

type Errors = OpenUnion
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
    -> UTCTime 
    -> m (Either Errors (AppId, Subdomain))
