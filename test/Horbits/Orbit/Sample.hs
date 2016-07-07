{-# LANGUAGE NoImplicitPrelude #-}

module Horbits.Orbit.Sample where

import           Control.Lens                hiding (elements, (*~))
import           Test.QuickCheck

import           Horbits.Body
import           Horbits.Dimensional.Prelude
import           Horbits.Orbit

data OrbitSample = OrbitSample { desc  :: String
                               , orbit :: Orbit
                               , sma   :: Length Double
                               , e     :: Dimensionless Double
                               , raan  :: Dimensionless Double
                               , incl  :: Dimensionless Double
                               , arg   :: Dimensionless Double
                               , ap    :: Length Double
                               , pe    :: Length Double
                               } deriving (Show)

sampleOrbits :: [OrbitSample]
sampleOrbits = [OrbitSample "Circular equatorial 100km"
                            (vectorOrbit Kerbin (v3 _0 _0 (sqrt hSq0)) (v3 _0 _0 _0) _0)
                            r0
                            _0
                            _0
                            _0
                            _0
                            alt0
                            alt0,

                OrbitSample "Circular equatorial retrograde 100km"
                            (vectorOrbit Kerbin (v3 _0 _0 (negate $ sqrt hSq0)) (v3 _0 _0 _0) _0)
                            r0
                            _0
                            _0
                            (180 *~ degree)
                            _0
                            alt0
                            alt0,

                OrbitSample "Elliptical (e = 0.2) equatorial with arg.pe = 0"
                            (vectorOrbit Kerbin
                                (v3 _0 _0 (sqrt $ 0.96 *~ one * hSq0))
                                (v3 (0.2 *~ one) _0 _0) _0)
                            r0
                            (0.2 *~ one)
                            _0
                            _0
                            _0
                            (240000 *~ meter)
                            ((-40000) *~ meter),

                OrbitSample "Circular 45 deg. incl, raan = 0"
                            (vectorOrbit Kerbin
                                (v3 _0
                                    (negate . sqrt $ 0.5 *~ one * hSq0)
                                    (sqrt $ 0.5 *~ one * hSq0))
                                (v3 _0 _0 _0)
                                _0)
                            r0
                            _0
                            _0
                            (45 *~ degree)
                            _0
                            alt0
                            alt0,

                OrbitSample "Circular 45 deg. incl, raan = 45 deg."
                            (vectorOrbit Kerbin
                                (v3 (sqrt $ 0.25 *~ one * hSq0)
                                    (negate . sqrt $ 0.25 *~ one * hSq0)
                                    (sqrt $ 0.5 *~ one * hSq0))
                                (v3 _0 _0 _0)
                                _0)
                            r0
                            _0
                            (45 *~ degree)
                            (45 *~ degree)
                            _0
                            alt0
                            alt0,

                OrbitSample "Elliptical (e = 0.2) 45 deg. incl, raan = arg. pe = 0"
                            (vectorOrbit Kerbin
                                (v3 _0
                                    (negate . sqrt $ 0.48 *~ one * hSq0)
                                    (sqrt $ 0.48 *~ one * hSq0))
                                (v3 (0.2 *~ one) _0 _0)
                                _0)
                            r0
                            (0.2 *~ one)
                            _0
                            (45 *~ degree)
                            _0
                            (240000 *~ meter)
                            ((-40000) *~ meter),

                OrbitSample "Elliptical (e = 0.2) 45 deg. incl, raan = 0, arg. pe = 90 deg."
                            (vectorOrbit Kerbin
                                (v3 _0
                                    (negate . sqrt $ 0.48 *~ one * hSq0)
                                    (sqrt $ 0.48 *~ one * hSq0))
                                (v3 _0
                                    (0.2 *~ one * sqrt (0.5 *~ one))
                                    (0.2 *~ one * sqrt (0.5 *~ one)))
                                _0)
                            r0
                            (0.2 *~ one)
                            _0
                            (45 *~ degree)
                            (90 *~ degree)
                            (240000 *~ meter)
                            ((-40000) *~ meter),

                OrbitSample "Elliptical (e = 0.2) 45 deg. incl, raan = 45 deg., arg.pe = 0"
                            (vectorOrbit Kerbin
                                (v3
                                    (sqrt $ 0.24 *~ one * hSq0)
                                    (negate . sqrt $ 0.24 *~ one * hSq0)
                                    (sqrt $ 0.48 *~ one * hSq0))
                                (v3 (0.2 *~ one * sqrt (0.5 *~ one))
                                    (0.2 *~ one * sqrt (0.5 *~ one))
                                    _0)
                                _0)
                            r0
                            (0.2 *~ one)
                            (45 *~ degree)
                            (45 *~ degree)
                            _0
                            (240000 *~ meter)
                            ((-40000) *~ meter)

                           ]
  where
    alt0 = 100000 *~ meter
    r0 = kerbin ^. bodyRadius + alt0
    hSq0 = r0 * kerbin ^. bodyGravitationalParam
    kerbin = getBody Kerbin

genSampleOrbits :: Gen OrbitSample
genSampleOrbits = elements sampleOrbits
