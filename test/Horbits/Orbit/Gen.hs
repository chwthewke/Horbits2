{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Horbits.Orbit.Gen where

import           Control.Lens                hiding ((*~), _1, _2)
import           Data.Maybe                  (fromMaybe)
import           Test.QuickCheck
import           System.Random

import           Horbits.Body
import           Horbits.Dimensional.Prelude
import           Horbits.Orbit

anyBody :: Gen BodyId
anyBody = arbitraryBoundedEnum

chooseQuantity :: (Random a, RealFloat a) => (Quantity d a, Quantity d a) -> Gen (Quantity d a)
chooseQuantity (lo, hi) = do
  val <- choose (0.0, 1.0)
  let inter = val *~ one
  return $ inter * lo + (_1 - inter) * hi

mkOrth :: Quantity d' (V3 Double) -> Quantity d (V3 Double) -> Quantity d' (V3 Double)
mkOrth v ref = stabilize $ if norm ref == _0 then v else refUnit `cross` v
  where refUnit = ref ^/ norm ref
        stabilize v' = if nearZeroOf (norm v) (norm v') then zero else v'

withNorm :: (Metric f, ((d' / d) * d) ~ d') => Quantity d (f Double) -> Quantity d' Double -> Quantity d' (f Double)
withNorm v n =
    if norm v == _0
      then zero
      else (n / norm v) *^ v

sphericalV3 :: (Quantity d Double, Quantity d Double) -> Gen (Quantity d (V3 Double))
sphericalV3 (lo, hi) = do
  n <- chooseQuantity (lo, hi)
  colat <- chooseQuantity (_0, pi)
  long <- chooseQuantity (_0, _2 * pi)
  return $ v3 (sin colat * cos long * n) (sin colat * sin long * n) (cos colat * n)

randomOrbit :: BodyId ->
                 (Quantity DSpecificAngularMomentum Double, Quantity DSpecificAngularMomentum Double) ->
                 (Dimensionless Double, Dimensionless Double) ->
                 Gen Orbit
randomOrbit body (loH, hiH) (loE, hiE) = do
        h <- sphericalV3 (loH, hiH)
        e0 <- sphericalV3 (_1, _1)
        eN <- chooseQuantity (loE, hiE) `suchThat` (< _1)
        let e = e0 `mkOrth` h `withNorm` eN
        return $ vectorOrbit body h e _0

stdRandomOrbit :: BodyId -> Gen Orbit
stdRandomOrbit body = randomOrbit body (loH, hiH) (_0, _1)
  where loH = sqrt (body ^. fromBodyId . bodyGravitationalParam * minAlt)
        hiH = sqrt (_2 * body ^. fromBodyId . bodyGravitationalParam  * minAlt)
        minAlt = body  ^. fromBodyId . bodyRadius +
                 fromMaybe _0 (body ^? fromBodyId . bodyAtmosphere . atmosphereHeight)

capturedOrbit :: BodyId -> Gen Orbit
capturedOrbit bId = do
  ap <- chooseQuantity (minR, maxR)
  pe <- chooseQuantity (minR, ap)
  let sma = (ap + pe) / _2
  let ecc = ap / sma - _1
  raan <- chooseQuantity (_0, tau)
  incl <- chooseQuantity (_0, pi)
  argPe <- chooseQuantity (_0, tau)
  maae <- chooseQuantity (_0, tau)
  return $ Orbit bId sma ecc raan incl argPe maae
  where body = getBody bId
        minR = body ^. bodyRadius + fromMaybe _0 (body ^? bodyAtmosphere . atmosphereHeight)
        maxR = fromMaybe (1e12 *~ meter) (body ^? bodySphereOfInfluence)


