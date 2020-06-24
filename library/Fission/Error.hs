module Fission.Error
  ( openLeft
  , relaxedLeft
  , fromMaybe
  , fromMaybe'
  , retryOnErr
  , module Fission.Error.Types
  ) where

import Fission.Prelude hiding (fromMaybe)
import Fission.Error.Types


openLeft :: IsMember err errs => err -> Either (OpenUnion errs) a
openLeft err = Left (openUnionLift err)

relaxedLeft :: Contains errsA errsB => OpenUnion errsA -> Either (OpenUnion errsB) a
relaxedLeft = Left . relaxOpenUnion

fromMaybe ::
  IsMember err errs
  => err
  -> (a -> b)
  -> Maybe a
  -> Either (OpenUnion errs) b
fromMaybe err okHandler = maybe (openLeft err) (Right . okHandler)

fromMaybe' :: IsMember err errs => err -> Maybe a -> Either (OpenUnion errs) a
fromMaybe' err = fromMaybe err identity

retryOnErr ::
  Monad m
  => (a -> m Bool)
  -> Natural
  -> m a
  -> m a
retryOnErr check times action = do
  result <- action
  check result >>= \case
    True -> return result
    False -> 
      if times > 0
        then retryOnErr check (times - 1) action
        else return result
