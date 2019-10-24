-- | Pin files via the CLI
module Fission.CLI.Pin
  ( pin
  , run
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has

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
import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success
import           RIO.List                    (headMaybe)

import Control.Monad.Except

maybeToEither:: Maybe val -> err -> Either err val
maybeToEither maybeA err = case maybeA of
  Just val -> Right val
  Nothing  -> Left err

run :: MonadRIO          cfg m
    => MonadUnliftIO         m
    => HasLogFunc        cfg
    => HasProcessContext cfg
    => Has Client.Runner cfg
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => CID
    -> UserConfig
    -> m (Either SomeException CID)
-- run cid@(CID hash) userConfig = Exception.handleWith_ CLI.Error.put' $ do
run cid@(CID hash) userConfig = do -- runExceptT do
  -- Client.Runner runner <- Config.get

  -- let
  --   maybePeer = headMaybe $ peers userConfig

  -- peer <- return $ maybeToEither maybePeer (toException "ERROR")

  -- result  <- liftE $ IPFS.Peer.connect peer
  -- hold <- liftE $ pin runner userConfig cid
  -- CLI.Success.live hash
  -- return $ Right cid

  Client.Runner runner <- Config.get

  logDebug $ "Remote pinning " <> display hash

  -- IPFS.Peer.connect IPFS.Peer.fission

  let
    mayPeer = headMaybe $ peers userConfig

  peer <- ExceptT $ return $ maybeToEither mayPeer (toException MyError)
  return $ IPFS.Peer.connect peer
  bar <- liftE $ pin runner userConfig cid
  -- CLI.Success.live hash
  -- return cid

  -- case foo' of
  --   Left err -> return $ Left err
  --   Right pr -> do
  --     IPFS.Peer.connect pr

  --     bar <- pin runner userConfig cid

  --     case bar of
  --       Right _ -> do
  --         CLI.Success.live hash
  --         return $ Right cid

  --       Left err -> do
  --         CLI.Error.put' err
  --         return $ Left $ toException err

pin :: MonadUnliftIO m => (ClientM NoContent -> m (Either ClientError NoContent)) -> UserConfig -> CID -> m (Either ClientError NoContent)
pin runner userConfig cid = do
  let auth = toBasicAuth userConfig
  CLI.withLoader 50000 . runner $ Fission.pin (Fission.request auth) cid

-- MonadIO

-- liftIO :: IO a -> m a

data MyErrrrrrrrror = MyError
  deriving (Show, Exception)

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
