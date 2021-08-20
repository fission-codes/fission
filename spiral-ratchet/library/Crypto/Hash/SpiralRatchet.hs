module Crypto.Hash.SpiralRatchet where

import           Crypto.Hash
import           Crypto.Hash.SpiralRatchet.Types
import           Crypto.Random

import qualified Data.Bits                       as Binary
import           Data.ByteArray

import           RIO
import qualified RIO.ByteString                  as BS

advance :: SpiralRatchet -> SpiralRatchet
advance SpiralRatchet {..} = do
  case (small == smallMax, medium == mediumMax) of
    (False, _) ->
      SpiralRatchet {small = step small, ..}

    (True, False) ->
      let
        mediumZero = step medium
        mediumMax  = stepTimes 255 mediumZero

        smallSeed = step . complement $ SpiralSeed medium
        smallZero = step smallSeed
        smallMax  = stepTimes 255 smallZero

      in
        SpiralRatchet
          { medium = mediumZero
          , small  = smallZero
          , ..
          }

    (True, True) ->
      let
        largeZero  = step large

        mediumSeed = step . complement $ SpiralSeed large
        mediumZero = step mediumSeed
        mediumMax' = stepTimes 255 mediumZero

        smallSeed  = step . complement $ SpiralSeed medium
        smallZero  = step smallSeed
        smallMax'  = stepTimes 255 smallZero

      in
        SpiralRatchet
          { large  = largeZero

          , medium    = mediumZero
          , mediumMax = mediumMax'

          , small    = smallZero
          , smallMax = smallMax'
          , ..
          }

------------

newtype SpiralSeed = SpiralSeed { sha256 :: Digest SHA256 }
  deriving newtype (Eq, Ord)

---------

step :: ByteArrayAccess ba => ba -> Digest SHA256
step bs = hashWith SHA256 bs

complement :: SpiralSeed -> ByteString
complement (SpiralSeed digestSeed) =
  digestSeed
    & convert
    & BS.unpack
    & fmap Binary.complement
    & BS.pack

genSeed :: (MonadIO m) => m SpiralSeed
genSeed = do
  bs :: ByteString <- liftIO $ getRandomBytes 32
  return . SpiralSeed $ step bs

fromSeed :: (MonadIO m, MonadThrow m) => SpiralSeed -> m SpiralRatchet
fromSeed (SpiralSeed initSeed) = do
  let
    largeZero = step initSeed

    mediumSeed = step . complement $ SpiralSeed initSeed
    mediumZero = step mediumSeed
    mediumMax  = stepTimes 255 mediumZero

    smallSeed = step . complement $ SpiralSeed mediumSeed
    smallZero = step smallSeed
    smallMax  = stepTimes 255 smallZero

  large  <- randForward largeZero
  medium <- randForward mediumZero
  small  <- randForward smallZero

  return SpiralRatchet {..}

stepTimes :: Natural -> Digest SHA256 -> Digest SHA256
stepTimes 0 sha = sha
stepTimes n sha = stepTimes (n - 1) (step sha)

randForward :: (MonadIO m, MonadThrow m) => Digest SHA256 -> m (Digest SHA256)
randForward sha = do
  bs :: ByteString <- liftIO $ getRandomBytes 1
  case uncons bs of
    Nothing        -> throwM Nope
    Just (byte, _) -> return $ stepTimes (fromIntegral byte) sha

gen :: (MonadIO m, MonadThrow m) => m SpiralRatchet
gen = fromSeed =<< genSeed

data Nope = Nope
  deriving (Show, Exception)
