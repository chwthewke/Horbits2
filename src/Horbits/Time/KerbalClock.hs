{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Horbits.Time.KerbalClock(KerbalClock(..), readClock, stopClock, startClock) where

import           Control.Lens                      hiding ((*~))
import qualified Prelude                           as P
import           System.Clock

import           Horbits.Dimensional.Prelude
import           Horbits.Time.KerbalDateTime


data TimeFunction = TimeFunction { _timeFunctionBase  :: KerbalInstant
                                 , _timeFunctionRate  :: Dimensionless Double
                                 , _timeFunctionStart :: TimeSpec
                                 } deriving (Show, Eq)

data KerbalClock = StoppedClock KerbalInstant
                 | RunningClock TimeFunction
                 deriving (Show, Eq)

makePrisms ''KerbalClock

runTimeFunction :: TimeFunction -> TimeSpec -> KerbalInstant
runTimeFunction (TimeFunction base rate start) now = base .+^ KerbalTime (rate * elapsed start now)

elapsed :: TimeSpec -> TimeSpec -> Time Double
elapsed (TimeSpec s1 ns1) (TimeSpec s2 ns2) =
    deltaS + deltaNs
  where
    deltaS = fromIntegral (s2 P.- s1) *~ second
    deltaNs = fromIntegral (ns2 P.- ns1) *~ nano second

readClock :: KerbalClock -> TimeSpec -> KerbalInstant
readClock (StoppedClock i) = const i
readClock (RunningClock tf) = runTimeFunction tf

stopClock :: KerbalClock -> TimeSpec -> KerbalClock
stopClock (RunningClock tf) = StoppedClock . runTimeFunction tf
stopClock s = const s

startClock :: KerbalClock -> Dimensionless Double -> TimeSpec -> KerbalClock
startClock clock rate now = RunningClock $ TimeFunction (readClock clock now) rate now
