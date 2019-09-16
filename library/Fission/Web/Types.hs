module Fission.Web.Types
  ( Host (..)
  , Port (..)
  ) where

import RIO

import qualified Network.Wai.Handler.Warp as Warp
import           System.Envy

newtype Host = Host { getHost :: Text }
  deriving          ( Eq
                    , Show
                    , Generic
                    )
  deriving newtype  IsString

instance FromEnv Host where
  fromEnv _ = Host <$> env "HOST"

newtype Port = Port { port :: Warp.Port }
  deriving          ( Show
                    , Eq
                    , Generic
                    )

instance FromEnv Port where
  fromEnv _ = Port <$> env "PORT"
