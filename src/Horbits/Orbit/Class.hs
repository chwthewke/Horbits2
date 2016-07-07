{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Horbits.Orbit.Class
    (Orbit(Orbit), OrbitClass(..),
    vectorOrbit,
    orbitParent, orbitMu,
    orbitAngularMomentumVector, orbitAngularMomentum, orbitEccentricityVector)
  where

import           Control.Lens                hiding (_1, _2)
import           Language.Haskell.TH.Syntax  (mkName)

import           Horbits.Body
import           Horbits.Dimensional.Prelude
-- Definitions and classes

-- TODO add vector definition to record ?

data Orbit = Orbit { _orbitParentBodyId                  :: BodyId
                   , _orbitSemiMajorAxis                 :: Length Double
                   , _orbitEccentricity                  :: Dimensionless Double
                   , _orbitRightAscensionOfAscendingNode :: Dimensionless Double
                   , _orbitInclination                   :: Dimensionless Double
                   , _orbitArgumentOfPeriapsis           :: Dimensionless Double
                   , _orbitMeanAnomalyAtEpoch            :: Dimensionless Double
                   } deriving (Show, Eq)

makeLensesWith (classyRules & lensClass .~ const (Just (mkName "OrbitClass", mkName "toOrbit"))) ''Orbit

-- Class equivalence

_orbitRotation :: OrbitClass t => t -> Rotation Double
_orbitRotation = do
    raan <- view orbitRightAscensionOfAscendingNode
    incl <- view orbitInclination
    argp <- view orbitArgumentOfPeriapsis
    return $ rotZ raan * rotX incl * rotZ argp

_angularMomentum :: OrbitClass t => t -> SpecificAngularMomentum Double
_angularMomentum = do
    a <- view orbitSemiMajorAxis
    mu <- view orbitMu
    e <- view orbitEccentricity
    return $ sqrt $ a * mu * (_1 - e ^ pos2)


orbitAngularMomentum :: OrbitClass t => Getter t (SpecificAngularMomentum Double)
orbitAngularMomentum = to _angularMomentum


_angularMomentumVector :: OrbitClass t => t -> SpecificAngularMomentum (V3 Double)
_angularMomentumVector = do
    rotation <- _orbitRotation
    h <- _angularMomentum
    return $ rotate rotation $ v3 _0 _0 h


orbitAngularMomentumVector :: OrbitClass t => Getter t (SpecificAngularMomentum (V3 Double))
orbitAngularMomentumVector = to _angularMomentumVector


_eccentricityVector :: OrbitClass t => t -> Dimensionless (V3 Double)
_eccentricityVector = do
    rotation <- _orbitRotation
    e <- view orbitEccentricity
    return $ rotate rotation $ v3 e _0 _0

orbitEccentricityVector :: OrbitClass t => Getter t (Dimensionless (V3 Double))
orbitEccentricityVector = to _eccentricityVector

-- Properties


orbitParent :: OrbitClass t => Getter t Body
orbitParent = orbitParentBodyId . fromBodyId

-- MU

orbitMu :: OrbitClass t => Getter t (GravitationalParameter Double)
orbitMu = orbitParent . bodyGravitationalParam

-- Define orbit by its angular momentum and eccentricity vectors

vectorOrbit :: BodyId -- ^ parent
            -> SpecificAngularMomentum (V3 Double) -- ^ angular momentum vector
            -> Dimensionless (V3 Double) -- ^ eccentricity vector
            -> Dimensionless Double -- ^ mean anomaly at epoch
            -> Orbit
vectorOrbit b h e = Orbit b a e' r i p
  where
    mu = b ^. fromBodyId . bodyGravitationalParam
    e' = norm e
    a = quadrance h / (mu * (_1 - e' ^ pos2))
    r = atan2' (h ^. _x) (negate $ h ^. _y)
    i = atan2' (norm $ h ^. _xy) (h ^. _z)
    p = atan2'
                (quadrance (h ^. _xy) * (e ^. _z) - ((h ^. _xy) `dot` (e ^. _xy)) * (h ^. _z))
                (norm h * (h ^. _x * e ^. _y - h ^. _y * e ^. _x))








