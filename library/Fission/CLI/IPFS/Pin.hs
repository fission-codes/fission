-- | Pin files via the CLI
module Fission.CLI.IPFS.Pin (add) where

import           Network.IPFS.CID.Types
import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.IPFS as Fission

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success

add ::
  ( MonadUnliftIO                       m
  , MonadLogger                         m
  , MonadWebRequest                 req m
  , MonadAuthedEndpoint Fission.Pin req
  )
  => CID
  -> m (Either ClientError CID)
add cid@(CID hash) = do
  logDebug $ "Remote pinning " <> display hash

  response <- CLI.withLoader 50000 $
    sendRequest . withAuth ucanJWT $ toEndpoint' Fission.pin

  case response of
    Right _ -> do
      CLI.Success.live hash
      return $ Right cid

    Left err -> do
      CLI.Error.put' err
      return $ Left err
