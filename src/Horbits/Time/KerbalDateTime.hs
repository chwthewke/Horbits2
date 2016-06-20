{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Horbits.Time.KerbalDateTime where

import Numeric.Units.Dimensional.SIUnits (second, minute, hour)
import Numeric.Units.Dimensional.UnitNames (UnitName, atom)

import Horbits.Dimensional.Prelude

newtype KerbalTime' a = KerbalTime { kerbalTime :: Time a }

newtype KerbalInstant' a = KerbalInstant { kerbalInstant :: Time a }

epoch :: Num a => KerbalInstant' a
epoch = undefined

(.-.) :: Num a => KerbalInstant' a -> KerbalInstant' a -> KerbalTime' a
ins .-. ins' = undefined

(.-^) :: Num a => KerbalInstant' a -> KerbalTime' a -> KerbalTime' a
ins .-^ dur = undefined

(.+^) :: Num a => KerbalInstant' a -> KerbalTime' a -> KerbalTime' a
ins .+^ dur = undefined

-- TODO custom units kday, kyear

nKday :: UnitName 'NonMetric
nKday = atom "Kd" "Kd" "Kerbal day"

nKYear :: UnitName 'NonMetric
nKYear = atom "Kyr" "Kyr" "Kerbal year"

kday :: Num a => Unit 'NonMetric DTime a
kday = mkUnitZ nKday 6 hour

kyear :: Num a => Unit 'NonMetric DTime a
kyear = mkUnitZ nKYear 426 kday
