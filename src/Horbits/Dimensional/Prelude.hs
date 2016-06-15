{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Horbits.Dimensional.Prelude(zero, (^*~), unQuantity) where

import Control.Lens
import Linear(Additive, Epsilon, Metric, R1, R2, R3, V1, V2, V3)
import Numeric.Units.Dimensional (Dimensionless, Quantity, Unit)
import Numeric.Units.Dimensional.Coercion (Dimensional(Quantity))
import Prelude (Bool, Fractional, Functor, Num, Real, RealFloat, flip, fmap, ($), (.))

import qualified Data.Fixed (mod')
import qualified Linear as L
import qualified Numeric.Units.Dimensional as D
import qualified Numeric.Units.Dimensional.Coercion as D
import qualified Numeric.NumType.DK.Integers as D (TypeInt(Pos2))
import qualified Prelude as P

infixl 6 ^+^, ^-^
infixl 7 ^*~, ^*, *^, ^/

unQuantity :: Quantity d a -> a
unQuantity = D.coerce

liftQ :: (a -> b) -> Quantity d1 a -> Quantity d2 b
liftQ = D.coerce

liftQ2 :: (a -> b -> c) -> Quantity d1 a -> Quantity d2 b -> Quantity d3 c
liftQ2 = D.coerce

liftQ3 :: (a -> b -> c -> d) -> Quantity d1 a -> Quantity d2 b -> Quantity d3 c -> Quantity d4 d
liftQ3 = D.coerce

isoDim :: Iso (Quantity d a) (Quantity d b) a b
isoDim = iso unQuantity Quantity

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
zero = Quantity L.zero

(^*~) :: (Functor f, Num a) => f a -> Unit m d a -> Quantity d (f a)
v ^*~ u = Quantity $ fmap (s P.*) v
  where s = unQuantity $ 1 D.*~ u

(^+^) :: (L.Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^+^) = liftQ2 (L.^+^)

(^-^) :: (L.Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^-^) = liftQ2 (L.^-^)

-- TODO here: use the lawless Functor instance instead?
(^*) :: (Num a, Functor f) => Quantity d (f a) -> Quantity d' a -> Quantity (d D.* d') (f a)
(^*) = liftQ2 (L.^*)

(*^) :: (Num a, Functor f) => Quantity d a -> Quantity d' (f a) -> Quantity (d D.* d') (f a)
(*^) = liftQ2 (L.*^)

(^/) :: (Fractional a, Functor f) => Quantity d (f a) -> Quantity d' a -> Quantity (d D./ d') (f a)
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

dot :: (Metric f, Num a) => Quantity d (f a) -> Quantity d' (f a) -> Quantity (d D.* d') a
dot = liftQ2 L.dot

qd :: (Metric f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity (d D.^ 'D.Pos2) a
qd = liftQ2 L.qd