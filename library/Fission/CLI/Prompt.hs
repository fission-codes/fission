module Fission.CLI.Prompt
  ( reask
  , reaskNotEmpty
  , reaskNotEmpty'
  , reaskYN
  , reaskWithError
  ) where

import           Fission.Prelude
import           RIO.ByteString as BS hiding (map, pack)
import qualified Fission.Internal.UTF8 as UTF8
import qualified Data.List as L
import           Data.Function

reaskWithError ::
  ( MonadIO m, MonadLogger m )
  => Text
  -> (ByteString -> Bool)
  -> (() -> m ())
  -> m ByteString
reaskWithError prompt check showError = do
  UTF8.putText prompt
  resp <- getLine
  if check resp
    then
      return resp
    else do
      showError()
      reaskWithError prompt check showError

-- | Continues prompting the user until they put in a valid response
reask ::
  ( MonadIO m, MonadLogger m )
  => Text
  -> (ByteString -> Bool)
  -> m ByteString
reask prompt check = reaskWithError prompt check (const (return ()))

-- | reask where valid responses are at least 1 character long
reaskNotEmpty ::
  ( MonadIO m, MonadLogger m )
  => Text
  -> m ByteString
reaskNotEmpty prompt = reask prompt \resp -> BS.length resp > 0

-- | reask where valid responses are at least 1 character long
reaskNotEmpty' ::
  ( MonadIO m, MonadLogger m )
  => Text
  -> m Text
reaskNotEmpty' prompt = do
  resp <- reaskNotEmpty prompt
  case decodeUtf8' resp of
    Right txt -> return txt
    Left _ -> do 
      UTF8.putTextLn "🔣 Oops, we couldn't read your input. Try again!"
      reaskNotEmpty' prompt

-- | reask where valid responses are some form of yes/no
reaskYN ::
  ( MonadIO m, MonadLogger m )
  => Text
  -> m Bool
reaskYN prompt = reask prompt ynTest
  >>= return . isYes

ynTest :: ByteString -> Bool
ynTest resp = isYes resp || isNo resp

isYes :: ByteString -> Bool
isYes resp = L.elem resp (["y", "Y", "yes", "Yes"] :: [ByteString])

isNo :: ByteString -> Bool
isNo resp = L.elem resp (["n", "N", "no", "No"] :: [ByteString])
