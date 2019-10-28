module Fission.CLI.Config.Types
  ( CommandM
  , Config (..)
  , UserConfig (..)
  , fissionAPI
  , logFunc
  , toBasicAuth
  ) where

import RIO
import RIO.Process (ProcessContext, HasProcessContext (..))

import Data.Has
import Data.Aeson
import Control.Lens (makeLenses)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Lazy
import Options.Applicative as OA
import Servant

import qualified Fission.Web.Client.Types as Client
import qualified Fission.IPFS.Types       as IPFS
import Fission.Internal.Orphanage.ByteString.Lazy ()

-- | The User specific Fission CLI config
data UserConfig = UserConfig
  { username :: ByteString
  , password :: ByteString
  -- , url      :: ByteString
  , peers    :: NonEmpty IPFS.Peer
  }
  deriving          ( Eq
                    , Show
                    )


instance ToJSON UserConfig where
  toJSON UserConfig {..} = object
    [ "username"      .= username
    , "password"  .= password
    , "peers" .= peers
    ]

instance FromJSON UserConfig where
  parseJSON = withObject "UserConfig" \obj ->
    UserConfig <$> obj .: "username"
               <*> obj .: "password"
               <*> obj .: "peers"

toBasicAuth :: UserConfig -> BasicAuthData
toBasicAuth usrCfg = BasicAuthData (usrCfg & username) (usrCfg & password)

-- | The action to attach to the command interface and description
type CommandM a = ExceptT a (Writer (Mod CommandFields a)) ()

-- | The configuration used for the CLI application
data Config = Config
  { _fissionAPI  :: !Client.Runner
  , _logFunc     :: !LogFunc
  , _processCtx  :: !ProcessContext
  , _ipfsPath    :: !IPFS.BinPath
  , _ipfsTimeout :: !IPFS.Timeout
  , _userConfig  :: UserConfig -- This one
  }

makeLenses ''Config

instance HasLogFunc Config where
  logFuncL = logFunc

instance Has Client.Runner Config where
  hasLens = fissionAPI

instance HasProcessContext Config where
  processContextL = processCtx

instance Has IPFS.BinPath Config where
  hasLens = ipfsPath

instance Has IPFS.Timeout Config where
  hasLens = ipfsTimeout

instance Has UserConfig Config where
  hasLens = userConfig



