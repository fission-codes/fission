-- | Pin files via the CLI
module Fission.CLI.Pin
  ( pin
  , run
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Data.List.NonEmpty

import Servant
import Servant.Client

import qualified Fission.Config              as Config
import           Fission.Internal.Constraint

import Fission.Internal.Exception as Exception

import           Fission.IPFS.CID.Types
import qualified Fission.IPFS.Peer      as IPFS.Peer
import qualified Fission.IPFS.Types     as IPFS

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission

import           Fission.CLI.Config.Types
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success

import Control.Monad.Except

-- maybeToEither:: Maybe val -> err -> Either err val
-- maybeToEither maybeA err = case maybeA of
--   Just val -> Right val
--   Nothing  -> Left err

run :: MonadRIO          cfg m
    => MonadUnliftIO         m
    => HasLogFunc        cfg
    => HasProcessContext cfg
    => Has Client.Runner cfg
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => CID
    -> NonEmpty IPFS.Peer
    -> BasicAuthData
    -> m (Either SomeException CID)
run cid@(CID hash) peers auth = runExceptT do
  logDebug $ "Remote pinning " <> display hash
  Client.Runner runner <- Config.get

  liftE $ IPFS.Peer.connect IPFS.Peer.fission

  IPFS.Peer.connect $ head $ peers
  liftE $ pin runner cid auth

  CLI.Success.live hash
  return cid

pin :: MonadUnliftIO m
    => (ClientM NoContent -> m (Either ClientError NoContent))
    -> CID
    -> BasicAuthData
    -> m (Either ClientError NoContent)
pin runner cid auth =
  CLI.withLoader 50000 . runner $ Fission.pin (Fission.request auth) cid

-- toException :: err -> SomeException

{-
   doTHing
   `catch` \case
     BadSender -> ...
     Timeout -> ....

-}

-- data EmailErrs = BadSender | Timeout | NoBody
-- data DBErrs = CantConnect | DuplicateKey

-- data MailingLIstErrs
--   = Email EmailErrs
--   | DB DBErrs


{-
data AnalyticsErrs = ...
-}

  -- [1,2,3] >>= \x ->
  --   [x + 1] >>= \y ->
  --     [x * 10, y * 100] >>= \z ->
  --       [z]

  -- do
  --   x <- [1,2,3]
  --   y <- [x + 1] -- [2,3,4]
  --   z <- [x * 10, y *100] -- [10, 20, 30, 200, 300, 400]
  --   [z]

  -- instance Monad Maybe where
  --   (=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
  --   (=<<) :: (a -> IO b) -> IO a -> IO b
  --   (=<<) :: (a -> [b]) -> [a] -> [b]
  --   return :: a -> m a

  --   Nothing >>= f = Nothing
  --   Just x >>= f = f x

  --   return val = Just val
