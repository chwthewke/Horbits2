module Horbits.Dimensional.Internal where

import           Control.Lens (Iso, iso)
import           Numeric.Units.Dimensional               (Quantity)
import           Numeric.Units.Dimensional.Coercion      (Dimensional(Quantity), coerce)

quantity :: a -> Quantity d a
quantity = coerce

unQuantity :: Quantity d a -> a
unQuantity = coerce

liftQ :: (a -> b) -> Quantity d1 a -> Quantity d2 b
liftQ = coerce

liftQ2 :: (a -> b -> c) -> Quantity d1 a -> Quantity d2 b -> Quantity d3 c
liftQ2 = coerce

liftQ3 :: (a -> b -> c -> d) -> Quantity d1 a -> Quantity d2 b -> Quantity d3 c -> Quantity d4 d
liftQ3 = coerce

isoDim :: Iso (Quantity d a) (Quantity d b) a b
isoDim = iso unQuantity Quantity
