module Fission.CLI.IPFS.Version.Types (Version (..)) where

import           Servant.API

import           Fission.Prelude

data Version = Version
  { major :: Word8
  , minor :: Word8
  , patch :: Word8
  }
  deriving (Show, Eq)

instance Display Version where
  display Version {..} =
    mconcat
      [ "v"
      , display @Int $ fromIntegral major
      , "."
      , display @Int $ fromIntegral minor
      , "."
      , display @Int $ fromIntegral patch
      ]

instance ToHttpApiData Version where
  toUrlPiece = textDisplay
