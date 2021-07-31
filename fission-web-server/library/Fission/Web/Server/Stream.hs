module Fission.Web.Server.Stream (toSourceT) where

import           Servant.Types.SourceT
import qualified Streamly.Prelude      as Streamly

import           Fission.Prelude

toSourceT :: Monad m => Streamly.SerialT m a -> SourceT m a
toSourceT serialStream =
  SourceT \k ->
    k $ Effect do
      cont <- Streamly.foldr folder Skip serialStream
      return $ cont Stop
  where
    folder :: a -> (StepT m a -> StepT m a) -> (StepT m a -> StepT m a)
    folder x contAcc = \nextCont -> contAcc (Yield x nextCont)
