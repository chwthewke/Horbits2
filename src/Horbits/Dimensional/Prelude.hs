{-# LANGUAGE NoImplicitPrelude #-}

module Horbits.Dimensional.Prelude(zero, (^*~), unQuantity) where

import qualified Prelude as P
import qualified Linear as L
import qualified Numeric.Units.Dimensional as D
import qualified Numeric.Units.Dimensional.Coercion as D


unQuantity :: D.Quantity d a -> a
unQuantity = D.coerce

zero :: (L.Additive f, P.Num a) => D.Quantity d (f a)
zero = D.Quantity L.zero

(^*~) :: (L.Additive f, P.Num a) => f a -> D.Unit m d a -> D.Quantity d (f a)
v ^*~ u = D.Quantity P.$ P.fmap (s P.*) v
  where s = unQuantity P.$ 1 D.*~ u
