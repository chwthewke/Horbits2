module Horbits.Matchers.Lib(equal, isA) where

import Horbits.Matchers.Matcher

-- Lib

equal :: (Show a, Eq a) => a -> Matcher a
equal expected = propMatcher (== expected) (show expected)

isA :: Show a => String -> (a -> Maybe b) -> Matcher1 a b
isA name f = defaultMatcher f ("a " ++ name)
