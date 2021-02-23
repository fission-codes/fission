module Fission.Unit.Prefix.Types
  ( Unity (..)
  , Deci  (..)
  , Centi (..)
  , Milli (..)
  , Micro (..)
  , Pico  (..)
  ) where

import           RIO

import           Fission.Unit.Prefix.Class

newtype Unity n = Unity { getUnity :: n }
  deriving         (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

instance Display n => Display (Unity n) where
  display (Unity n) = display n

instance FromPrefixed Unity n where
  fromPrefixed (Unity n) = n

instance ToPrefixed Unity n where
  toPrefixed n = Unity n

--

newtype Deci n = Deci { getDeci :: n }
  deriving         (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

instance Display n => Display (Deci n) where
  display (Deci n) = display n <> "d"

instance Fractional n => FromPrefixed Deci n where
  fromPrefixed (Deci n) = n / 10

instance Num n => ToPrefixed Deci n where
  toPrefixed n = Deci $ n * 10

--

newtype Centi n = Centi { getCenti :: n }
  deriving         (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

instance Display n => Display (Centi n) where
  display (Centi n) = display n <> "c"

instance Fractional n => FromPrefixed Centi n where
  fromPrefixed (Centi n) = n / 100

instance Num n => ToPrefixed Centi n where
  toPrefixed n = Centi $ n * 100

--

newtype Milli n = Milli { getMilli :: n }
  deriving          (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

instance Display n => Display (Milli n) where
  display (Milli n) = display n <> "m"

instance Fractional n => FromPrefixed Milli n where
  fromPrefixed (Milli n) = n / 1_000

instance Num n => ToPrefixed Milli n where
  toPrefixed n = Milli $ n * 1_000

--

newtype Micro n = Micro { getMicro :: n }
  deriving          (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

instance Display n => Display (Micro n) where
  display (Micro n) = display n <> "μ"

instance Fractional n => FromPrefixed Micro n where
  fromPrefixed (Micro n) = n / 1_000_000

instance Num n => ToPrefixed Micro n where
  toPrefixed n = Micro $ n * 1_000_000

--

newtype Nano n = Nano { getNano :: n }
  deriving          (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

instance Display n => Display (Nano n) where
  display (Nano n) = display n <> "n"

instance Fractional n => FromPrefixed Nano n where
  fromPrefixed (Nano n) = n / 1_000_000_000

instance Num n => ToPrefixed Nano n where
  toPrefixed n = Nano $ n * 1_000_000_000

--

newtype Pico n = Pico { getPico :: n }
  deriving          (Functor, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Floating, Fractional, RealFloat, RealFrac, Integral)

instance Display n => Display (Pico n) where
  display (Pico n) = display n <> "p"

instance Fractional n => FromPrefixed Pico n where
  fromPrefixed (Pico n) = n / 1_000_000_000_000

instance Num n => ToPrefixed Pico n where
  toPrefixed n = Pico $ n * 1_000_000_000_000

