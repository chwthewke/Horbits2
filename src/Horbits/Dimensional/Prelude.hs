{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}

module Horbits.Dimensional.Prelude(module Horbits.Dimensional.Prelude, module X) where

import           Control.Lens
import qualified Data.Fixed                        (mod')
import           Linear                            (Additive, Conjugate,
                                                    Epsilon, Metric, Quaternion,
                                                    R1, R2, R3, V1, V2, V3)
import qualified Linear                            as L
import qualified Numeric.NumType.DK.Integers       as D (TypeInt (Neg1, Pos2, Zero))
import           Numeric.Units.Dimensional         (Dimensionless, Quantity,
                                                    Unit, (*), (/), (^))
import qualified Numeric.Units.Dimensional         as D
import           Prelude                           (Bool, Floating, Fractional,
                                                    Functor, Num, Real,
                                                    RealFloat, flip, fmap, ($),
                                                    (.))
import qualified Prelude                           as P

import           Linear                            as X hiding (axisAngle,
                                                         cross, distance, dot,
                                                         nearZero, norm,
                                                         normalize, project, qd,
                                                         quadrance, rotate,
                                                         signorm, zero, (*^),
                                                         (^*), (^+^), (^-^),
                                                         (^/), _x, _xy, _xz, _y,
                                                         _yx, _yz, _z, _zx, _zy)
import           Numeric.Units.Dimensional.Prelude as X hiding (subtract, zero,
                                                         (^/))
import           Prelude                           as X hiding (abs, acos,
                                                         acosh, asin, asinh,
                                                         atan, atan2, atanh,
                                                         cos, cosh, exp, log,
                                                         mod, negate, pi, sin,
                                                         sinh, sqrt, subtract,
                                                         sum, tan, tanh, (*),
                                                         (**), (+), (-), (/),
                                                         (^))

import           Horbits.Dimensional.Internal

infixl 6 ^+^, ^-^
infixl 7 ^*~, ^*, *^, ^/

-- Quantities

-- TODO rename? see standard quantity names
type DSpecificAngularMomentum = 'Dim 'D.Pos2 'D.Zero 'D.Neg1 'D.Zero 'D.Zero 'D.Zero 'D.Zero
type SpecificAngularMomentum = Quantity DSpecificAngularMomentum


-- Extra numeric operators

subtract :: Num a => Quantity d a -> Quantity d a -> Quantity d a
subtract = flip (D.-)

mod' :: Real a => Quantity d a -> Quantity d a -> Quantity d a
mod' = liftQ2 Data.Fixed.mod'

atan2' :: (RealFloat a) => Quantity d a -> Quantity d a -> Dimensionless a
atan2' = liftQ2 atan2'
    where atan2' y' (-0) = P.atan2 y' 0
          atan2' y' x' = P.atan2 y' x'

-- Vector operators

zero :: (Additive f, Num a) => Quantity d (f a)
zero = quantity L.zero

(^*~) :: (Functor f, Num a) => f a -> Unit m d a -> Quantity d (f a)
v ^*~ u = quantity $ fmap (s P.*) v
  where s = unQuantity $ 1 D.*~ u

(^/~) :: (Functor f, Fractional a) => Quantity d (f a) -> Unit m d a -> f a
v ^/~ u =  fmap (P./ s) . unQuantity $ v
  where s = unQuantity $ 1 D.*~ u

(^+^) :: (L.Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^+^) = liftQ2 (L.^+^)

(^-^) :: (L.Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^-^) = liftQ2 (L.^-^)

(^*) :: (Num a, Functor f) => Quantity d (f a) -> Quantity d' a -> Quantity (d * d') (f a)
(^*) = liftQ2 (L.^*)

(*^) :: (Num a, Functor f) => Quantity d a -> Quantity d' (f a) -> Quantity (d * d') (f a)
(*^) = liftQ2 (L.*^)

(^/) :: (Fractional a, Functor f) => Quantity d (f a) -> Quantity d' a -> Quantity (d / d') (f a)
(^/) = liftQ2 (L.^/)

-- Vector lenses

quantityLens :: (Functor f) => Lens' a b -> (Quantity d b -> f (Quantity d b)) -> (Quantity d a -> f (Quantity d a))
quantityLens l = isoDim . l . from isoDim

_x :: (R1 f) => Lens' (Quantity d (f a)) (Quantity d a)
_x = quantityLens L._x

_y :: (R2 f) => Lens' (Quantity d (f a)) (Quantity d a)
_y =  quantityLens L._y

_xy :: (R2 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_xy =  quantityLens L._xy

_yx :: (R2 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_yx =  quantityLens L._yx

_z :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d a)
_z =  quantityLens L._z

_xz :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_xz =  quantityLens L._xz

_zx :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_zx =  quantityLens L._zx

_yz :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_yz =  quantityLens L._yz

_zy :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_zy =  quantityLens L._zy

-- Vector constructors

v2 :: Quantity d a -> Quantity d a -> Quantity d (V2 a)
v2 = liftQ2 L.V2

v3 :: Quantity d a -> Quantity d a -> Quantity d a -> Quantity d (V3 a)
v3 = liftQ3 L.V3

-- Epsilon

nearZero :: Epsilon a => Dimensionless a -> Bool
nearZero = L.nearZero . unQuantity

nearZeroOf :: (Epsilon a, Fractional a) => Quantity d a -> Quantity d a -> Bool
nearZeroOf u q = nearZero $ q D./ u

-- Vector operations

dot :: (Metric f, Num a) => Quantity d (f a) -> Quantity d' (f a) -> Quantity (d * d') a
dot = liftQ2 L.dot

cross :: (Num a) => Quantity d (V3 a) -> Quantity d' (V3 a) -> Quantity (d * d') (V3 a)
cross = liftQ2 L.cross

qd :: (Metric f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity (d ^ 'D.Pos2) a
qd = liftQ2 L.qd

quadrance :: (Metric f, Num a) => Quantity d (f a) -> Quantity (d ^ 'D.Pos2) a
quadrance = liftQ L.quadrance

distance :: (Metric f, Floating a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d a
distance = liftQ2 L.distance

norm :: (Metric f, Floating a) => Quantity d (f a) -> Quantity d a
norm = liftQ L.norm

signorm :: (Metric f, Floating a) => Quantity d (f a) -> Dimensionless (f a)
signorm = liftQ L.signorm

normalize :: (Metric f, Floating a, Epsilon a) => Quantity d (f a) -> Dimensionless (f a)
normalize = liftQ L.normalize

project :: (Metric f, Fractional a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
project = liftQ2 L.project

-- Quaternion

type Rotation a = Dimensionless (Quaternion a)

rotate :: (Conjugate a, RealFloat a) =>
          Rotation a -> Quantity d (V3 a) -> Quantity d (V3 a)
rotate = liftQ2 L.rotate

axisAngle :: (Floating a, Epsilon a) => Quantity d (V3 a) -> Dimensionless a -> Rotation a
axisAngle = liftQ2 L.axisAngle

rotX :: (Floating a, Epsilon a) => Dimensionless a -> Rotation a
rotX = axisAngle $ v3 D._1 D._0 D._0

rotZ :: (Floating a, Epsilon a) => Dimensionless a -> Rotation a
rotZ = axisAngle $ v3 D._0 D._0 D._1
