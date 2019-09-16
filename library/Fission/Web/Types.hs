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
  deriving anyclass FromEnv
  deriving newtype  IsString

newtype Port = Port { port :: Warp.Port }
  deriving          ( Show
                    , Eq
                    , Generic
                    )
  deriving anyclass FromEnv
