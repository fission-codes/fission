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

-- instance FromHttpApiData Version where
--   parseUrlPiece txt =
--     case Text.uncons txt of
--       Just ('v', tail) ->
--         case Text.split (== '.') tail of
--           segments@[_major, _minor, _patch] ->
--             case readMaybe . Text.unpack <$> segments of
--               [Just major, Just minor, Just patch] ->
--                 Right Version {..}
--
--               _ ->
--                 Left "Invalid segment(s)"
--
--           _ ->
--             Left "Version has wrong number of segments"
--
--       Just _  ->
--         Left "Version did not start with 'v'"
--
--       Nothing ->
--         Left "Empty version"
