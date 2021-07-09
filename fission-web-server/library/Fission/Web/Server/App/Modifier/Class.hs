module Fission.Web.Server.App.Modifier.Class
  ( Modifier (..)
  , Errors'
  ) where

import qualified Network.IPFS.Add.Error                             as IPFS.Pin
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Get.Error                             as IPFS.Stat

import           Servant.API
import           Servant.Server

import           Fission.Prelude                                    hiding (on)

import           Fission.Error                                      as Error
import           Fission.URL

import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import           Fission.Web.Server.Models

-- FIXME onlu the bytesrecieved; extract out!
import           Fission.Web.API.App.Update.Streaming.Types

type Errors' = OpenUnion
  '[ NotFound App
   , NotFound AppDomain
   , NotFound Domain
   , NotFound URL

   , ActionNotAuthorized App
   , ActionNotAuthorized AppDomain
   , ActionNotAuthorized URL

   , IPFS.Pin.Error
   , IPFS.Stat.Error

   , ServerError
   , InvalidURL
   ]

class Monad m => Modifier m where
  setCID ::
       UserId  -- ^ User for auth
    -> URL     -- ^ URL associated with target app
    -> CID     -- ^ New CID
    -> Bool    -- ^ Flag: copy data (default yes)
    -> UTCTime -- ^ Now
    -> m (Either Errors' AppId)

  setCIDStreaming ::
       UserId
    -> URL
    -> CID
    -> UTCTime
    -> m (SourceIO BytesReceived)
