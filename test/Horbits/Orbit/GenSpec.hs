{-# LANGUAGE NoImplicitPrelude #-}

module Horbits.Orbit.GenSpec where

import           Control.Lens                hiding ((*~), _1)
import           Data.Maybe                  (fromMaybe)
import           Test.Hspec
import           Test.QuickCheck

import           Horbits.Dimensional.Prelude
import           Horbits.Body
import           Horbits.Orbit
import           Horbits.Orbit.Gen


-- TMP
import Horbits.Matchers


-- properties of generators

spec :: Spec
spec = describe "Orbit.Gen" $ do
    describe "standard random orbits" $ do
        it "are elliptical" $
            forAll (anyBody >>= stdRandomOrbit) orbitIsElliptical
        it "have orthogonal h and e vectors" $
            forAll (anyBody >>= stdRandomOrbit) orbitHasOrthogonalHAndE
    describe "captured orbits" $ do
        it "are elliptical" $
            forAll (anyBody >>= capturedOrbit) orbitIsElliptical
        it "have orthogonal h and e vectors" $
            forAll (anyBody >>= capturedOrbit) orbitHasOrthogonalHAndE
        it "have a periapsis above their bodies atmosphere (or surface)" $
            forAll (anyBody >>= capturedOrbit) orbitHasPeAboveAtmo
        it "have an apoapsis below their bodies SoI edge" $
            forAll (anyBody >>= capturedOrbit) orbitHasApBelowSoI

orbitHasOrthogonalHAndE :: Orbit -> Bool
orbitHasOrthogonalHAndE orbit = e `dot` h < 1e-12 *~ one * (norm e * norm h)
  where e = orbit ^. orbitEccentricityVector
        h = orbit ^. orbitAngularMomentumVector

orbitIsElliptical :: Orbit -> Bool
orbitIsElliptical orbit = orbit ^. orbitEccentricity < _1

orbitHasPeAboveAtmo :: Orbit -> Bool
orbitHasPeAboveAtmo orbit = orbit ^. orbitPeriapsisAltitude >= safeAltitude
  where safeAltitude = fromMaybe _0 (orbit ^? orbitParent . bodyAtmosphere . atmosphereHeight)

orbitHasApBelowSoI :: Orbit -> Bool
orbitHasApBelowSoI orbit = maybe True (orbit ^. orbitApoapsis <) soi
  where soi = orbit ^? orbitParent . bodySphereOfInfluence