module Fission.Error
  ( openLeft
  , relaxedLeft
  , fromMaybe
  , fromMaybe'
  , module Fission.Error.Types
  ) where

import Data.WorldPeace
import RIO hiding (fromMaybe)

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
fromMaybe' err = fromMaybe err id
