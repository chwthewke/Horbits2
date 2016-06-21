{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Horbits.Time.KerbalDateTime where

import           Data.Fixed                          (mod')
import           Numeric.Units.Dimensional.SIUnits   (hour, minute, second)
import           Numeric.Units.Dimensional.UnitNames (UnitName, atom)
import           Prelude                             (mod)

import           Horbits.Dimensional.Prelude         hiding (mod, mod')

-- Units

nKday :: UnitName 'NonMetric
nKday = atom "Kd" "Kd" "Kerbal day"

nKYear :: UnitName 'NonMetric
nKYear = atom "Kyr" "Kyr" "Kerbal year"

kday :: Num a => Unit 'NonMetric DTime a
kday = mkUnitZ nKday 6 hour

kyear :: Num a => Unit 'NonMetric DTime a
kyear = mkUnitZ nKYear 426 kday

-- temporal data type

data Temporal = Duration | Instant

newtype KerbalTime (k :: Temporal) (a :: *) =
    KerbalTime { time :: Time a }

type KerbalDuration' = KerbalTime 'Duration

type KerbalInstant' = KerbalTime 'Instant

kerbalDuration :: KerbalDuration' a -> Time a
kerbalDuration = time

kerbalInstant :: KerbalInstant' a -> Time a
kerbalInstant = time

type KerbalDuration = KerbalDuration' Double

type KerbalInstant = KerbalInstant' Double

(.-.) :: Num a => KerbalInstant' a -> KerbalInstant' a -> KerbalDuration' a
(KerbalTime ins) .-. (KerbalTime ins') = KerbalTime (ins - ins')

(.-^) :: Num a => KerbalInstant' a -> KerbalDuration' a -> KerbalInstant' a
(KerbalTime ins) .-^ (KerbalTime dur) = KerbalTime (ins - dur)

(.+^) :: Num a => KerbalInstant' a -> KerbalDuration' a -> KerbalInstant' a
(KerbalTime ins) .+^ (KerbalTime dur) = KerbalTime (ins + dur)

secondFraction :: RealFrac a => KerbalTime k a -> a
secondFraction t = (time t /~ second) `mod'` 1

unitsAtPosition :: (RealFrac a, Integral b) => Unit m DTime a -> b -> KerbalTime k a -> b
unitsAtPosition u m = (`mod` m) . floor . (/~ u) . time

seconds :: RealFrac a => KerbalTime k a -> Int
seconds = unitsAtPosition second 60

minutes :: RealFrac a => KerbalTime k a -> Int
minutes = unitsAtPosition minute 60

hours :: RealFrac a => KerbalTime k a -> Int
hours = unitsAtPosition hour 6

days :: RealFrac a => KerbalTime k a -> Int
days = unitsAtPosition kday 426

years :: RealFrac a => KerbalTime k a -> Int
years = floor . (/~ kyear) . time
