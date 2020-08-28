module Fission.App.Creator
  ( module Fission.App.Creator.Class
  , createWithPlaceholder
  , createDB
  ) where

import           Fission.Models
import           Fission.Prelude

import           Fission.App.Content         as AppCID
import           Fission.App.Creator.Class   as App

import           Fission.App.Creator.Class

import           Fission.URL.Subdomain.Types

import           Network.IPFS.Bytes.Types
import           Network.IPFS.CID.Types

import           Database.Esqueleto          hiding ((<&>))

createWithPlaceholder ::
  ( App.Creator        m
  , AppCID.Initializer m
  )
  => UserId
  -> Maybe Subdomain
  -> UTCTime
  -> m (Either Errors' (AppId, Subdomain))
createWithPlaceholder ownerId maySubdomain now = do
  defaultCID <- AppCID.placeholder
  create ownerId defaultCID maySubdomain now

createDB ::
     MonadIO m
  => UserId
  -> CID
  -> Bytes
  -> UTCTime
  -> Transaction m AppId
createDB ownerId cid size now = do
  appId <- insert App
    { appOwnerId    = ownerId
    , appCid        = cid
    , appSize       = size
    , appInsertedAt = now
    , appModifiedAt = now
    }

  _ <- insert $ CreateAppEvent appId ownerId cid size now
  return appId
