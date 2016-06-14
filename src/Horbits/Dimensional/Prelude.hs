{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

module Horbits.Dimensional.Prelude(zero, (^*~), unQuantity) where

import Control.Lens
import Linear(Additive)
import Numeric.Units.Dimensional (Dimensionless, Quantity, Unit)
import Numeric.Units.Dimensional.Coercion (Dimensional(Quantity))
import Prelude (Fractional, Functor, Num, Real, RealFloat, fmap, ($))

import qualified Data.Fixed (mod')
import qualified Linear as L
import qualified Numeric.Units.Dimensional as D
import qualified Numeric.Units.Dimensional.Coercion as D
import qualified Prelude as P

unQuantity :: Quantity d a -> a
unQuantity = D.coerce

liftQ :: (a -> a) -> Quantity d1 a -> Quantity d2 a
liftQ = D.coerce

liftQ2 :: (a -> a -> a) -> Quantity d1 a -> Quantity d2 a -> Quantity d3 a
liftQ2 = D.coerce

isoDim :: Iso' (Quantity d a) a
isoDim = iso unQuantity Quantity

-- Extra operators

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
(^*) v a = liftQ (L.^* (unQuantity a)) v

(*^) :: (Num a, Functor f) => Quantity d a -> Quantity d' (f a) -> Quantity (d D.* d') (f a)
(*^) a = liftQ ((unQuantity a) L.*^)

(^/) :: (Fractional a, Functor f) => Quantity d (f a) -> Quantity d' a -> Quantity (d D./ d') (f a)
(^/) v a = liftQ (L.^/ (unQuantity a)) v
