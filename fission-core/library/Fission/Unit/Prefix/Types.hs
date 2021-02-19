module Fission.Unit.Prefix.Types where

import           Data.Coerce
import qualified Data.Fixed  as Fixed
import           RIO

newtype Seconds n = Seconds { getSeconds :: n }
  deriving (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

class SISymbol unit where
  symbolFor :: unit a -> Utf8Builder

instance SISymbol Seconds where
  symbolFor (Seconds _) = "s"

prettyPrint :: forall prefix unit scalar .
  ( FromPrefixed prefix
  , SISymbol     prefix
  , SISymbol     unit
  , Display      scalar
  , Fractional   (unit scalar)
  , unit scalar `Coercible` scalar
  )
  => prefix (unit scalar)
  -> Utf8Builder
prettyPrint compound = display (coerce unprefixed :: scalar) <> prefixSym <> unitSym
    where
      prefixSym  = symbolFor compound
      unitSym    = symbolFor unprefixed
      unprefixed = fromPrefixed compound

--

newtype Unity n = Unity { getUnity :: n }
  deriving         (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

newtype Deci n = Deci { getDeci :: n }
  deriving         (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

newtype Centi n = Centi { getCenti :: n }
  deriving         (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

newtype Milli n = Milli { getMilli :: n }
  deriving          (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

newtype Micro n = Micro { getMicro :: n }
  deriving          (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

newtype Nano n = Nano { getNano :: n }
  deriving          (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

newtype Pico n = Pico { getPico :: n }
  deriving          (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

--

class FromPrefixed prefix where
  fromPrefixed :: Fractional n => prefix n -> n

class ToPrefixed prefix where
  toPrefixed :: Num n => n -> prefix n

instance FromPrefixed Unity where
  fromPrefixed (Unity n) = n

instance ToPrefixed Unity where
  toPrefixed n = Unity n

instance FromPrefixed Centi where
  fromPrefixed (Centi n) = n / 10

instance ToPrefixed Centi where
  toPrefixed n = Centi $ n * 10

instance FromPrefixed Milli where
  fromPrefixed (Milli n) = n / 100

instance ToPrefixed Milli where
  toPrefixed n = Milli $ n * 100

instance FromPrefixed Micro where
  fromPrefixed (Micro n) = n / 1_000_000

instance ToPrefixed Micro where
  toPrefixed n = Micro $ n * 1_000_000

instance FromPrefixed Nano where
  fromPrefixed (Nano n) = n / 1_000_000_000

instance ToPrefixed Nano where
  toPrefixed n = Nano $ n * 1_000_000_000

instance FromPrefixed Pico where
  fromPrefixed (Pico n) = n / 1_000_000_000_000

instance ToPrefixed Pico where
  toPrefixed n = Pico $ n * 1_000_000_000_000

mapScalar :: (Functor prefix, Functor unit) => (a -> b) -> prefix (unit a) -> prefix (unit b)
mapScalar f n = fmap f <$> n

changePrefix :: (Fractional a, FromPrefixed n, ToPrefixed m) => n a -> m a
changePrefix = toPrefixed . fromPrefixed

asFixed ::
  ( Functor  prefix
  , Functor  unit
  , Integral n
  )
  => prefix (unit n)
  -> prefix (unit (Fixed.Fixed 1))
asFixed n = mapScalar fromIntegral n

asNatural ::
  ( Functor  prefix
  , Functor  unit
  , RealFrac n
  )
  => prefix (unit n)
  -> prefix (unit Natural)
asNatural n = mapScalar truncate n

toInt ::
  ( Functor      unit
  , Functor      prefix
  , FromPrefixed prefix
  , Integral     n
  , RealFrac     (unit (Fixed.Fixed 1))
  )
  => prefix (unit n)
  -> Int
toInt compund = round $ fromPrefixed (asFixed compund)
