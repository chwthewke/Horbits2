{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Horbits.Time.KerbalClock where

import           Control.Lens                      hiding ((*~))
-- TODO include SIUnits in Horbits.Dimensional.Prelude, others?
import           Numeric.Units.Dimensional.SIUnits (second)
import           System.Clock

import           Horbits.Dimensional.Prelude
import           Horbits.Time.KerbalDateTime


data TimeFunction = TimeFunction { _timeFunctionBase  :: KerbalInstant
                                 , _timeFunctionRate  :: Double
                                 , _timeFunctionStart :: TimeSpec
                                 } deriving (Show, Eq)

data KerbalClock = StoppedClock KerbalInstant
                 | RunningClock TimeFunction
                 deriving (Show, Eq)

makePrisms ''KerbalClock

runTimeFunction :: TimeFunction -> TimeSpec -> KerbalInstant
runTimeFunction (TimeFunction base rate start) now = base .+^ KerbalTime ((rate * elapsed start now) *~ second)
  where elapsed (TimeSpec s1 ns1) (TimeSpec s2 ns2) = fromIntegral (s2 - s1) + 1e-9 * fromIntegral (ns2 - ns1)

