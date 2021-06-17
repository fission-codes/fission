module Fission.Web.Server.IPFS.DNSLink.Class
  ( MonadDNSLink (..)
  , Errors'
  ) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude                                    hiding (set)

import           Fission.Error.Types
import           Fission.URL

import           Fission.Web.Server.Error.ActionNotAuthorized.Types

import           Fission.Web.Server.AWS.Route53.Class
import           Fission.Web.Server.AWS.Types                       as AWS

import           Fission.Web.Server.Models

type Errors' = OpenUnion
  '[ ServerError
   , InvalidURL

   , NotFound            URL
   , ActionNotAuthorized URL
   ]

-- | Low-level 'DNSLink' interface
class MonadRoute53 m => MonadDNSLink m where
  set ::
       UserId     -- ^ Who is performing this action (for auth)
    -> URL        -- ^ The @URL@ target
    -> AWS.ZoneID -- ^ The @ZoneID@ for the associated @URL@
    -> CID        -- ^ The @CID@ to set at that DNSLink
    -> m (Either Errors' URL)

  unset ::
       UserId     -- ^ Who is performing this action (for auth)
    -> URL        -- ^ The @URL@ target
    -> AWS.ZoneID -- ^ The @ZoneID@ for the associated @URL@
    -> m (Either Errors' ())

  follow ::
       UserId
    -> URL        -- ^ Follower
    -> AWS.ZoneID -- ^ Follower Zone
    -> Path URL   -- ^ Followee
    -> m (Either Errors' ())
