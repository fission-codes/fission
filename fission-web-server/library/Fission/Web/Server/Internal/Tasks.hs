-- | Helper tasks for support
module Fission.Web.Server.Internal.Tasks
  ( deleteByUsername
  , ensureAllPinned
  , pinAllToCluster
  , getAllDBPins
  , listPins
  ) where

import qualified RIO.List                                  as List
import qualified RIO.Map                                   as Map
import qualified RIO.Text                                  as Text

import           Database.Esqueleto

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Client                       as IPFS
import qualified Network.IPFS.URL.Types                    as IPFS

import           Servant.API
import           Servant.Client

import           Fission.Prelude

import           Fission.User.Username.Types

import qualified Fission.Web.Server.IPFS.Cluster           as Cluster
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB
import           Fission.Web.Server.Types
import qualified Fission.Web.Server.User                   as User

import           Fission.Web.Server.Internal.Orphanage.CID ()

deleteByUsername :: Text -> Server ()
deleteByUsername userNameTxt =
  case mkUsername userNameTxt of
    Left _ ->
      error "Invalid username"

    Right uname -> do
      logDebug $ "👻 Deleting user: " <> textDisplay uname
      User.getByUsername uname >>= \case
        Just (Entity userId _) -> runDB $ deleteCascade userId
        Nothing                -> error "User doesn't exist"

---

ensureAllPinned :: Server ()
ensureAllPinned = do
  dbCIDs      <- getAllDBPins
  clusterURLs <- asks ipfsURLs
  manager     <- asks ipfsHttpManager

  forM_ clusterURLs \(IPFS.URL url) -> do
    let clientManager = mkClientEnv manager url

    liftIO (runClientM listPins $ mkClientEnv manager url) >>= \case
      Left err ->
        logError $ "Pin list request failed: " <> displayShow err

      Right (ListPinsResponse (PinLsList remoteCIDMap)) ->  do
        let
          remoteCIDs  = Map.keys remoteCIDMap
          missingCIDs = dbCIDs List.\\ remoteCIDs

        forM_ missingCIDs \cid@(CID hash) ->
          liftIO (runClientM (IPFS.pin hash) clientManager) >>= \case
            Left err ->
              logError $ mconcat
                [ "Pin failed: "
                , " -- "
                , displayShow url
                , " -- "
                , displayShow cid
                , " -- "
                , displayShow err
                ]

            Right _ -> do
              logInfo $ "Pinned " <> display cid
              noop

pinAllToCluster :: [CID] -> Server [(CID, ClientError)]
pinAllToCluster cids =
  foldM combiner mempty cids
  where
    combiner acc cid =
      Cluster.pin cid >>= \case
        Left err -> do
          logWarn $ "Unable to pin " <> display err
          return ((cid, err) : acc)

        Right _ ->
          return acc

getAllDBPins :: Server [CID]
getAllDBPins =
  runDB do
    appRoots  <- select $ from \app      -> return (app      ^. AppCid)
    userRoots <- select $ from \user     -> return (user     ^. UserDataRoot)
    loosePins <- select $ from \loosePin -> return (loosePin ^. LoosePinCid)

    return $ fmap unValue (appRoots ++ userRoots ++ loosePins)

listPins :: ClientM ListPinsResponse
listPins = (client (Proxy @ListPins)) (Just Recursive)

type ListPins
  = "api"
  :> "v0"
  :> "pin"
  :> "ls"
  :> QueryParam "type" PinType
  :> Post '[JSON] PinlsList

instance FromJSON PinLsList where
  parseJSON = withObject "PinLsList" \obj -> do
    keyMap <- obj .: "Keys"
    return (PinLsList keyMap)

data PinType
  = Indirect
  | Recursive
  deriving (Show, Eq)

instance FromJSON PinType where
  parseJSON = withText "PinType" \txt ->
    case txt of
      "indirect"  -> return Indirect
      "recursive" -> return Recursive
      other       -> fail $ Text.unpack (other <> " is not a valid PinType")

instance ToHttpApiData PinType where
  toUrlPiece Indirect  = "indirect"
  toUrlPiece Recursive = "recursive"
