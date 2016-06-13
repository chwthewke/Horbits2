{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Horbits.Dimensional.Prelude(zero, (^*~)) where

import qualified Prelude as P
import qualified Linear as L
import qualified Numeric.Units.Dimensional as D

zero :: (L.Additive f, P.Num a) => D.Quantity d (f a)
zero = D.dmap (P.const L.zero) D._0

(^*~) :: (L.Additive f, P.Num a) => f a -> D.Unit m d a -> D.Quantity d (f a)
v ^*~ u = P.undefined

unitApprox :: (P.Num a) => D.Unit m d a -> a
unitApprox = P.undefined
